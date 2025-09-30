import java.nio.file._
import java.nio.channels.FileChannel
import java.nio.ByteBuffer
import scala.collection.concurrent.TrieMap
import scala.util.{Try, Success, Failure}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.concurrent.atomic.AtomicInteger

sealed trait FileState
case class Unchanged(path: String, hash: Int) extends FileState
case class Modified(path: String, oldHash: Int, newHash: Int) extends FileState
case class Deleted(path: String, oldHash: Int) extends FileState
case class Added(path: String, newHash: Int) extends FileState
case class ErrorFile(path: String, error: String) extends FileState
case class EmptyFile(path: String) extends FileState

class BitStreamProcessor(val buffer: ByteBuffer) extends AnyVal {
  def processXor: Int = {
    var result = 0
    while (buffer.remaining() >= 2) {
      result ^= buffer.getShort() & 0xFFFF
    }
    if (buffer.hasRemaining) {
      val lastByte = buffer.get() & 0xFF
      result ^= (lastByte << 8)
    }
    result
  }
}

sealed trait FileOperation[+A] {
  def map[B](f: A => B): FileOperation[B] = this match {
    case FileSuccess(value) => FileSuccess(f(value))
    case FileError(msg) => FileError(msg)
  }

  def flatMap[B](f: A => FileOperation[B]): FileOperation[B] = this match {
    case FileSuccess(value) => f(value)
    case FileError(msg) => FileError(msg)
  }
}
case class FileSuccess[A](value: A) extends FileOperation[A]
case class FileError(message: String) extends FileOperation[Nothing]

case class FileInfo(
                     path: String,
                     hash: Int,
                     size: Long,
                     lastModified: String
                   )

trait HtmlGenerator {
  implicit class HtmlOps(s: String) {
    def wrapTag(tag: String): String = s"<$tag>$s</$tag>"
    def withClass(className: String): String = s"""<span class="$className">$s</span>"""
    def colorize(color: String): String = s"""<span style="color: $color;">$s</span>"""
  }

  def generateHtml(results: Seq[FileState]): String = {
    val timestamp = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))
    val content = results.map {
      case Unchanged(path, _) =>
        s"✓ $path".withClass("unchanged").colorize("green")
      case Modified(path, old, neu) =>
        s"⚠ $path [хэш изменился: $old → $neu]".withClass("modified").colorize("orange")
      case Deleted(path, _) =>
        s"✗ $path".withClass("deleted").colorize("red")
      case Added(path, _) =>
        s"+ $path".withClass("added").colorize("blue")
      case ErrorFile(path, error) =>
        s"⚡ $path [$error]".withClass("error").colorize("darkred")
      case EmptyFile(path) =>
        s"○ $path [пустой файл]".withClass("empty").colorize("gray")
    }.map(_.wrapTag("div")).mkString("\n")

    s"""
       |<!DOCTYPE html>
       |<html>
       |<head>
       |  <meta charset="UTF-8">
       |  <title>Отчёт ревизора диска - $timestamp</title>
       |  <style>
       |    body { font-family: monospace; background: #f0f0f0; padding: 20px; }
       |    .container { background: white; padding: 20px; border-radius: 10px; box-shadow: 0 0 10px rgba(0,0,0,0.1); }
       |    .header { font-size: 24px; font-weight: bold; margin-bottom: 20px; }
       |    .timestamp { color: #666; font-size: 14px; }
       |    div { padding: 5px; margin: 2px 0; }
       |    .unchanged { background: #e6ffe6; }
       |    .modified { background: #fff3cd; }
       |    .deleted { background: #ffe6e6; }
       |    .added { background: #e6f3ff; }
       |    .error { background: #ffcccc; }
       |    .empty { background: #f0f0f0; }
       |  </style>
       |</head>
       |<body>
       |  <div class="container">
       |    <div class="header">Отчёт ревизора диска</div>
       |    <div class="timestamp">Время проверки: $timestamp</div>
       |    <hr>
       |    $content
       |  </div>
       |</body>
       |</html>
    """.stripMargin
  }
}

object DiskAuditor extends App with HtmlGenerator {

  private val HASH_FILE_NAME = "hash-codes.json"
  private val REPORT_FILE_NAME = "audit-report.html"
  private val BATCH_SIZE = 100
  private val processedFiles = new AtomicInteger(0)
  private val totalFiles = new AtomicInteger(0)

  object JsonHelper {
    def toJson(files: Map[String, FileInfo]): String = {
      val entries = files.map { case (path, info) =>
        s"""    "$path": {
           |      "hash": ${info.hash},
           |      "size": ${info.size},
           |      "lastModified": "${info.lastModified}"
           |    }""".stripMargin
      }.mkString(",\n")

      s"""{\n$entries\n}"""
    }

    def fromJson(json: String): Map[String, FileInfo] = {
      val pattern = """"([^"]+)":\s*\{\s*"hash":\s*(\d+),\s*"size":\s*(\d+),\s*"lastModified":\s*"([^"]+)"\s*\}""".r
      pattern.findAllMatchIn(json).map { m =>
        val path = m.group(1)
        val info = FileInfo(path, m.group(2).toInt, m.group(3).toLong, m.group(4))
        path -> info
      }.toMap
    }
  }

  def calculateHash(file: Path): FileOperation[Int] = {
    Try {
      if (!Files.exists(file) || !Files.isReadable(file)) {
        FileError(s"Файл недоступен: $file")
      } else {
        val size = Files.size(file)
        if (size == 0) {
          FileSuccess(0)
        } else {
          val channel = FileChannel.open(file, StandardOpenOption.READ)
          try {
            val buffer = ByteBuffer.allocate(math.min(size.toInt, 1024 * 1024))
            var hash = 0

            while (channel.read(buffer) != -1) {
              buffer.flip()
              val processor = new BitStreamProcessor(buffer)
              hash ^= processor.processXor
              buffer.clear()
            }

            FileSuccess(hash)
          } finally {
            channel.close()
          }
        }
      }
    } match {
      case Success(result) => result
      case Failure(e) => FileError(s"Ошибка чтения: ${e.getMessage}")
    }
  }

  def walkDirectory(root: Path): List[Path] = {
    println(s"📂 Начинаем обход директории: $root")

    if (!Files.exists(root)) {
      println(s"❌ Директория не существует: $root")
      return List.empty
    }

    if (!Files.isDirectory(root)) {
      println(s"❌ Указанный путь не является директорией: $root")
      return List.empty
    }

    def walkRecursive(dir: Path, depth: Int = 0): List[Path] = {
      val indent = "  " * depth
      println(s"$indent🔍 Сканируем: ${dir.getFileName}")

      try {
        import scala.jdk.CollectionConverters._
        val entries = Files.list(dir).iterator().asScala.toList

        entries.flatMap { path =>
          if (Files.isDirectory(path)) {
            if (!Files.isSymbolicLink(path)) {
              path :: walkRecursive(path, depth + 1)
            } else {
              println(s"$indent⚠️ Пропускаем символическую ссылку: ${path.getFileName}")
              List.empty
            }
          } else if (!path.getFileName.toString.equals(HASH_FILE_NAME) &&
            !path.getFileName.toString.equals(REPORT_FILE_NAME)) {
            println(s"$indent📄 Найден файл: ${path.getFileName}")
            List(path)
          } else {
            List.empty
          }
        }
      } catch {
        case e: AccessDeniedException =>
          println(s"$indent❌ Нет доступа к директории: ${dir.getFileName} - ${e.getMessage}")
          List.empty
        case e: Exception =>
          println(s"$indent❌ Ошибка при обходе директории: ${dir.getFileName} - ${e.getMessage}")
          List.empty
      }
    }

    val files = walkRecursive(root)
    println(s"✅ Обход завершен. Найдено файлов: ${files.size}")
    files
  }

  def scanFiles(root: Path): Map[String, FileInfo] = {
    val resultMap = TrieMap[String, FileInfo]()
    val errorList = TrieMap[String, String]()
    val emptyFiles = TrieMap[String, Unit]()

    println("🚀 Starting file scan...")
    val files = walkDirectory(root)

    if (files.isEmpty) {
      println("⚠️ No files found for processing")
      return Map.empty
    }

    val totalFilesCount = files.size
    println(s"📊 Total files to process: $totalFilesCount")

    files.zipWithIndex.foreach { case (file, index) =>
      val relativePath = root.relativize(file).toString.replace('\\', '/')

      calculateHash(file) match {
        case FileSuccess(hash) =>
          val size = Files.size(file)
          val lastModified = Files.getLastModifiedTime(file).toString

          if (size == 0) {
            emptyFiles += (relativePath -> ())
          }

          resultMap += (relativePath -> FileInfo(relativePath, hash, size, lastModified))

          val processed = index + 1
          if (processed % 10 == 0 || processed == totalFilesCount) {
            val progress = (processed * 100) / totalFilesCount
            println(s"📈 Progress: $processed/$totalFilesCount ($progress%)")
          }

        case FileError(msg) =>
          errorList += (relativePath -> msg)
          val processed = index + 1
          if (processed % 10 == 0 || processed == totalFilesCount) {
            val progress = (processed * 100) / totalFilesCount
            println(s"📈 Progress: $processed/$totalFilesCount ($progress%)")
          }
      }
    }

    println(s"🏁 Scan completed. Total files processed: ${resultMap.size}")

    if (errorList.nonEmpty) {
      println(s"\n⚠️ Errors during file processing (${errorList.size}):")
      errorList.take(10).foreach { case (path, error) =>
        println(s"  - $path: $error")
      }
      if (errorList.size > 10) {
        println(s"  ... and ${errorList.size - 10} more errors")
      }
    }

    if (emptyFiles.nonEmpty) {
      println(s"\n○ Empty files found: ${emptyFiles.size}")
      emptyFiles.keys.take(5).foreach(path => println(s"  - $path"))
      if (emptyFiles.size > 5) {
        println(s"  ... and ${emptyFiles.size - 5} more empty files")
      }
    }

    resultMap.toMap
  }

  def compareStates(oldState: Map[String, FileInfo], newState: Map[String, FileInfo]): Seq[FileState] = {
    val allPaths = (oldState.keySet ++ newState.keySet).toSeq.sorted

    allPaths.map { path =>
      (oldState.get(path), newState.get(path)) match {
        case (Some(old), Some(neu)) if old.hash == neu.hash =>
          Unchanged(path, old.hash)
        case (Some(old), Some(neu)) =>
          Modified(path, old.hash, neu.hash)
        case (Some(old), None) =>
          Deleted(path, old.hash)
        case (None, Some(neu)) =>
          Added(path, neu.hash)
        case _ =>
          ErrorFile(path, "Неизвестное состояние")
      }
    }
  }

  def run(rootPath: String): Unit = {
    val startTime = System.currentTimeMillis()
    val root = Paths.get(rootPath).toAbsolutePath
    val hashFile = root.resolve(HASH_FILE_NAME)

    println(s"🔍 Disk auditor started for directory: ${root.toString}")
    println(s"⏰ Start time: ${LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))}")

    if (Files.exists(hashFile)) {
      println("📋 Hash file found. Starting integrity check...")

      val oldStateJson = new String(Files.readAllBytes(hashFile))
      val oldState = JsonHelper.fromJson(oldStateJson)
      println(s"📊 Files in database: ${oldState.size}")

      val newState = scanFiles(root)

      val comparison = compareStates(oldState, newState)

      val htmlReport = generateHtml(comparison)
      Files.write(root.resolve(REPORT_FILE_NAME), htmlReport.getBytes)

      val textReport = comparison.map {
        case Unchanged(path, _) => s"[OK] $path"
        case Modified(path, _, _) => s"[MODIFIED] $path"
        case Deleted(path, _) => s"[DELETED] $path"
        case Added(path, _) => s"[ADDED] $path"
        case ErrorFile(path, error) => s"[ERROR] $path: $error"
        case EmptyFile(path) => s"[EMPTY] $path"
      }.mkString("\n")

      Files.write(root.resolve("audit-report.txt"), textReport.getBytes)

      println(s"✅ Check completed. Reports saved to $REPORT_FILE_NAME and audit-report.txt")

    } else {
      println("🆕 First run. Creating hash database...")

      val state = scanFiles(root)

      if (state.nonEmpty) {
        val json = JsonHelper.toJson(state)
        Files.write(hashFile, json.getBytes)
        println(s"✅ Hash database created. Files processed: ${state.size}")
      } else {
        println("⚠️ No files to save in database")
      }
    }

    val elapsedTime = (System.currentTimeMillis() - startTime) / 1000.0
    println(f"⏱️ Execution time: $elapsedTime%.2f seconds")
  }

  run("random_project_xOTLoKa1")
}