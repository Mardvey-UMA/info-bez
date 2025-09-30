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
case class Renamed(oldPath: String, newPath: String, hash: Int) extends FileState
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

  case class DirectoryNode(name: String, files: List[FileState], subdirs: Map[String, DirectoryNode])

  def buildDirectoryTree(results: Seq[FileState]): DirectoryNode = {
    def insertIntoTree(node: DirectoryNode, pathParts: List[String], state: FileState): DirectoryNode = {
      pathParts match {
        case Nil => node
        case fileName :: Nil =>
          node.copy(files = state :: node.files)
        case dirName :: rest =>
          val subdir = node.subdirs.getOrElse(dirName, DirectoryNode(dirName, List.empty, Map.empty))
          val updatedSubdir = insertIntoTree(subdir, rest, state)
          node.copy(subdirs = node.subdirs + (dirName -> updatedSubdir))
      }
    }

    val root = DirectoryNode("root", List.empty, Map.empty)
    results.foldLeft(root) { (tree, state) =>
      val path = state match {
        case Unchanged(p, _) => p
        case Modified(p, _, _) => p
        case Deleted(p, _) => p
        case Added(p, _) => p
        case Renamed(_, newP, _) => newP
        case ErrorFile(p, _) => p
        case EmptyFile(p) => p
      }
      val pathParts = path.split('/').toList
      insertIntoTree(tree, pathParts, state)
    }
  }

  def renderDirectoryTree(node: DirectoryNode, level: Int = 0): String = {
    val indent = "&nbsp;&nbsp;&nbsp;&nbsp;" * level
    val dirHeader = if (level > 0) {
      s"""<div style="margin-top: 10px; font-weight: bold; color: #333;">$indent📁 ${node.name}</div>"""
    } else {
      ""
    }

    val filesHtml = node.files.sortBy {
      case Unchanged(p, _) => p
      case Modified(p, _, _) => p
      case Deleted(p, _) => p
      case Added(p, _) => p
      case Renamed(_, newP, _) => newP
      case ErrorFile(p, _) => p
      case EmptyFile(p) => p
    }.map { state =>
      val fileName = state match {
        case Unchanged(p, _) => p.split('/').last
        case Modified(p, _, _) => p.split('/').last
        case Deleted(p, _) => p.split('/').last
        case Added(p, _) => p.split('/').last
        case Renamed(oldP, newP, _) => s"${oldP.split('/').last} → ${newP.split('/').last}"
        case ErrorFile(p, _) => p.split('/').last
        case EmptyFile(p) => p.split('/').last
      }

      val content = state match {
        case Unchanged(_, _) =>
          s"$indent&nbsp;&nbsp;&nbsp;&nbsp;✓ $fileName".withClass("unchanged").colorize("green")
        case Modified(_, old, neu) =>
          s"$indent&nbsp;&nbsp;&nbsp;&nbsp;⚠ $fileName [хэш изменился: $old → $neu]".withClass("modified").colorize("orange")
        case Deleted(_, _) =>
          s"$indent&nbsp;&nbsp;&nbsp;&nbsp;✗ $fileName".withClass("deleted").colorize("red")
        case Added(_, _) =>
          s"$indent&nbsp;&nbsp;&nbsp;&nbsp;+ $fileName".withClass("added").colorize("blue")
        case Renamed(oldP, newP, _) =>
          s"$indent&nbsp;&nbsp;&nbsp;&nbsp;↻ Переименован".withClass("renamed").colorize("purple")
        case ErrorFile(_, error) =>
          s"$indent&nbsp;&nbsp;&nbsp;&nbsp;⚡ $fileName [$error]".withClass("error").colorize("darkred")
        case EmptyFile(_) =>
          s"$indent&nbsp;&nbsp;&nbsp;&nbsp;○ $fileName [пустой файл]".withClass("empty").colorize("gray")
      }
      content.wrapTag("div")
    }.mkString("\n")

    val subdirsHtml = node.subdirs.toSeq.sortBy(_._1).map { case (_, subdir) =>
      renderDirectoryTree(subdir, level + 1)
    }.mkString("\n")

    dirHeader + filesHtml + subdirsHtml
  }

  def generateHtml(results: Seq[FileState]): String = {
    val timestamp = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))
    val tree = buildDirectoryTree(results)
    val content = renderDirectoryTree(tree)

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
       |    .renamed { background: #f3e6ff; }
       |    .error { background: #ffcccc; }
       |    .empty { background: #f0f0f0; }
       |  </style>
       |</head>
       |<body>
       |  <div class="container">
       |    <div class="header">📋 Отчёт ревизора диска</div>
       |    <div class="timestamp">🕐 Время проверки: $timestamp</div>
       |    <hr>
       |    $content
       |  </div>
       |</body>
       |</html>
    """.stripMargin
  }
}

object DiskAuditor extends App with HtmlGenerator {

  private val OUTPUT_DIR_NAME = "disk_auditor_reports"
  private val HASH_FILE_NAME = "hash-codes.json"
  private val REPORT_FILE_NAME = "audit-report.html"
  private val TEXT_REPORT_NAME = "audit-report.txt"

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
      val pattern = """"([^"]+)":\s*\{\s*"hash":\s*(-?\d+),\s*"size":\s*(\d+),\s*"lastModified":\s*"([^"]+)"\s*\}""".r
      pattern.findAllMatchIn(json).map { m =>
        val path = m.group(1)
        val info = FileInfo(path, m.group(2).toInt, m.group(3).toLong, m.group(4))
        path -> info
      }.toMap
    }
  }

  def calculateHash(file: Path): FileOperation[Int] = {
    Try {
      if (Files.isDirectory(file)) {
        FileError("Это каталог")
      } else if (!Files.exists(file) || !Files.isReadable(file)) {
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
          val fileName = path.getFileName.toString

          if (Files.isDirectory(path)) {
            if (fileName != OUTPUT_DIR_NAME && !Files.isSymbolicLink(path)) {
              walkRecursive(path, depth + 1)
            } else if (fileName == OUTPUT_DIR_NAME) {
              println(s"$indent⚠️ Пропускаем директорию отчетов: $fileName")
              List.empty
            } else {
              println(s"$indent⚠️ Пропускаем символическую ссылку: $fileName")
              List.empty
            }
          } else {
            println(s"$indent📄 Найден файл: $fileName")
            List(path)
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

    println("🚀 Начинаем сканирование файлов...")
    val files = walkDirectory(root)

    if (files.isEmpty) {
      println("⚠️ Файлов для обработки не найдено")
      return Map.empty
    }

    val totalFilesCount = files.size
    println(s"📊 Всего файлов для обработки: $totalFilesCount")

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
            println(s"📈 Прогресс: $processed/$totalFilesCount ($progress%)")
          }

        case FileError(msg) =>
          errorList += (relativePath -> msg)
          val processed = index + 1
          if (processed % 10 == 0 || processed == totalFilesCount) {
            val progress = (processed * 100) / totalFilesCount
            println(s"📈 Прогресс: $processed/$totalFilesCount ($progress%)")
          }
      }
    }

    println(s"🏁 Сканирование завершено. Обработано файлов: ${resultMap.size}")

    if (errorList.nonEmpty) {
      println(s"\n⚠️ Ошибки при обработке файлов (${errorList.size}):")
      errorList.take(10).foreach { case (path, error) =>
        println(s"  - $path: $error")
      }
      if (errorList.size > 10) {
        println(s"  ... и еще ${errorList.size - 10} ошибок")
      }
    }

    if (emptyFiles.nonEmpty) {
      println(s"\n○ Найдено пустых файлов: ${emptyFiles.size}")
      emptyFiles.keys.take(5).foreach(path => println(s"  - $path"))
      if (emptyFiles.size > 5) {
        println(s"  ... и еще ${emptyFiles.size - 5} пустых файлов")
      }
    }

    resultMap.toMap
  }

  def compareStates(oldState: Map[String, FileInfo], newState: Map[String, FileInfo]): Seq[FileState] = {
    val oldPaths = oldState.keySet
    val newPaths = newState.keySet

    val oldHashToPath = oldState.groupBy(_._2.hash).map { case (hash, entries) =>
      hash -> entries.keys.toSet
    }
    val newHashToPath = newState.groupBy(_._2.hash).map { case (hash, entries) =>
      hash -> entries.keys.toSet
    }

    val renames = scala.collection.mutable.Set[(String, String, Int)]()
    val processedOld = scala.collection.mutable.Set[String]()
    val processedNew = scala.collection.mutable.Set[String]()

    oldState.foreach { case (oldPath, oldInfo) =>
      if (!newPaths.contains(oldPath) && oldInfo.hash != 0) {
        newHashToPath.get(oldInfo.hash).foreach { paths =>
          paths.foreach { newPath =>
            if (!oldPaths.contains(newPath) && !processedNew.contains(newPath)) {
              renames += ((oldPath, newPath, oldInfo.hash))
              processedOld += oldPath
              processedNew += newPath
            }
          }
        }
      }
    }

    val allPaths = (oldPaths ++ newPaths).toSeq.sorted

    allPaths.flatMap { path =>
      if (processedOld.contains(path) || processedNew.contains(path)) {
        None
      } else {
        (oldState.get(path), newState.get(path)) match {
          case (Some(old), Some(neu)) if old.hash == neu.hash =>
            Some(Unchanged(path, old.hash))
          case (Some(old), Some(neu)) =>
            Some(Modified(path, old.hash, neu.hash))
          case (Some(old), None) =>
            Some(Deleted(path, old.hash))
          case (None, Some(neu)) =>
            Some(Added(path, neu.hash))
          case _ =>
            Some(ErrorFile(path, "Неизвестное состояние"))
        }
      }
    } ++ renames.map { case (oldPath, newPath, hash) =>
      Renamed(oldPath, newPath, hash)
    }
  }

  def run(rootPath: String): Unit = {
    val startTime = System.currentTimeMillis()
    val root = Paths.get(rootPath).toAbsolutePath
    val outputDir = root.resolve(OUTPUT_DIR_NAME)

    if (!Files.exists(outputDir)) {
      Files.createDirectories(outputDir)
      println(s"📁 Создана директория для отчетов: $outputDir")
    }

    val hashFile = outputDir.resolve(HASH_FILE_NAME)
    val htmlReportFile = outputDir.resolve(REPORT_FILE_NAME)
    val textReportFile = outputDir.resolve(TEXT_REPORT_NAME)

    println(s"🔍 Ревизор диска запущен для директории: ${root.toString}")
    println(s"⏰ Время начала: ${LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))}")

    if (Files.exists(hashFile)) {
      println("📋 Файл хэшей найден. Начинаем проверку целостности...")

      val oldStateJson = new String(Files.readAllBytes(hashFile))
      val oldState = JsonHelper.fromJson(oldStateJson)
      println(s"📊 Файлов в базе данных: ${oldState.size}")

      val newState = scanFiles(root)

      val comparison = compareStates(oldState, newState)

      val htmlReport = generateHtml(comparison)
      Files.write(htmlReportFile, htmlReport.getBytes)

      val textReport = comparison.map {
        case Unchanged(path, _) => s"[БЕЗ ИЗМЕНЕНИЙ] $path"
        case Modified(path, _, _) => s"[ИЗМЕНЕН] $path"
        case Deleted(path, _) => s"[УДАЛЕН] $path"
        case Added(path, _) => s"[ДОБАВЛЕН] $path"
        case Renamed(oldPath, newPath, _) => s"[ПЕРЕИМЕНОВАН] $oldPath → $newPath"
        case ErrorFile(path, error) => s"[ОШИБКА] $path: $error"
        case EmptyFile(path) => s"[ПУСТОЙ] $path"
      }.mkString("\n")

      Files.write(textReportFile, textReport.getBytes)

      val stats = comparison.groupBy(_.getClass).map { case (cls, items) =>
        cls.getSimpleName -> items.size
      }

      println(s"\n📊 Статистика проверки:")
      stats.toSeq.sortBy(_._1).foreach { case (status, count) =>
        println(s"  - $status: $count")
      }

      println(s"\n✅ Проверка завершена. Отчеты сохранены:")
      println(s"  - HTML: $htmlReportFile")
      println(s"  - TXT: $textReportFile")

      val newStateJson = JsonHelper.toJson(newState)
      Files.write(hashFile, newStateJson.getBytes)
      println(s"💾 База данных хэшей обновлена")

    } else {
      println("🆕 Первый запуск. Создаем базу данных хэшей...")

      val state = scanFiles(root)

      if (state.nonEmpty) {
        val json = JsonHelper.toJson(state)
        Files.write(hashFile, json.getBytes)
        println(s"✅ База данных хэшей создана. Обработано файлов: ${state.size}")
        println(s"💾 Файл базы данных: $hashFile")
      } else {
        println("⚠️ Нет файлов для сохранения в базе данных")
      }
    }

    val elapsedTime = (System.currentTimeMillis() - startTime) / 1000.0
    println(f"\n⏱️ Время выполнения: $elapsedTime%.2f секунд")
  }

  run("random_project_xOTLoKa1")
}
