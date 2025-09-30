<img src="https://r2cdn.perplexity.ai/pplx-full-logo-primary-dark%402x.png" style="height:64px;margin-right:32px"/>

# Полное руководство по скрипту Disk Auditor на Scala

## Оглавление

1. [Общее описание проекта](#%D0%BE%D0%B1%D1%89%D0%B5%D0%B5-%D0%BE%D0%BF%D0%B8%D1%81%D0%B0%D0%BD%D0%B8%D0%B5-%D0%BF%D1%80%D0%BE%D0%B5%D0%BA%D1%82%D0%B0)
2. [Структура данных](#%D1%81%D1%82%D1%80%D1%83%D0%BA%D1%82%D1%83%D1%80%D0%B0-%D0%B4%D0%B0%D0%BD%D0%BD%D1%8B%D1%85)
3. [Алгоритм хеширования файлов](#%D0%B0%D0%BB%D0%B3%D0%BE%D1%80%D0%B8%D1%82%D0%BC-%D1%85%D0%B5%D1%88%D0%B8%D1%80%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F-%D1%84%D0%B0%D0%B9%D0%BB%D0%BE%D0%B2)
4. [Алгоритм обхода директорий](#%D0%B0%D0%BB%D0%B3%D0%BE%D1%80%D0%B8%D1%82%D0%BC-%D0%BE%D0%B1%D1%85%D0%BE%D0%B4%D0%B0-%D0%B4%D0%B8%D1%80%D0%B5%D0%BA%D1%82%D0%BE%D1%80%D0%B8%D0%B9)
5. [Алгоритм отслеживания изменений](#%D0%B0%D0%BB%D0%B3%D0%BE%D1%80%D0%B8%D1%82%D0%BC-%D0%BE%D1%82%D1%81%D0%BB%D0%B5%D0%B6%D0%B8%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F-%D0%B8%D0%B7%D0%BC%D0%B5%D0%BD%D0%B5%D0%BD%D0%B8%D0%B9)
6. [Алгоритм определения переименований](#%D0%B0%D0%BB%D0%B3%D0%BE%D1%80%D0%B8%D1%82%D0%BC-%D0%BE%D0%BF%D1%80%D0%B5%D0%B4%D0%B5%D0%BB%D0%B5%D0%BD%D0%B8%D1%8F-%D0%BF%D0%B5%D1%80%D0%B5%D0%B8%D0%BC%D0%B5%D0%BD%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D0%B9)
7. [Генерация HTML-отчетов](#%D0%B3%D0%B5%D0%BD%D0%B5%D1%80%D0%B0%D1%86%D0%B8%D1%8F-html-%D0%BE%D1%82%D1%87%D0%B5%D1%82%D0%BE%D0%B2)
8. [Особенности Scala](#%D0%BE%D1%81%D0%BE%D0%B1%D0%B5%D0%BD%D0%BD%D0%BE%D1%81%D1%82%D0%B8-scala)

***

## Общее описание проекта

**Disk Auditor** — это инструмент для мониторинга целостности файловой системы, написанный на чистой Scala без использования сторонних библиотек. Программа сканирует директорию, вычисляет хеши файлов и отслеживает изменения между запусками.[^1][^5]

### Основные возможности:

- Вычисление хеш-сумм файлов с использованием XOR-алгоритма
- Обнаружение добавленных, измененных, удаленных файлов
- Интеллектуальное определение переименованных файлов по хешу
- Генерация HTML и текстовых отчетов с древовидной структурой
- Хранение состояния в JSON-формате

***

## Структура данных

### Sealed trait FileState

```scala
sealed trait FileState
```

**Назначение**: Базовый sealed trait для всех возможных состояний файла.[^2][^1]

**Особенность Scala**: Использование `sealed trait` означает, что все наследники должны быть определены в том же файле. Это позволяет компилятору проверять exhaustive pattern matching (полноту сопоставления с образцом). Если вы забудете обработать один из случаев в `match`, компилятор выдаст предупреждение.

### Case классы состояний

```scala
case class Unchanged(path: String, hash: Int) extends FileState
case class Modified(path: String, oldHash: Int, newHash: Int) extends FileState
case class Deleted(path: String, oldHash: Int) extends FileState
case class Added(path: String, newHash: Int) extends FileState
case class Renamed(oldPath: String, newPath: String, hash: Int) extends FileState
case class ErrorFile(path: String, error: String) extends FileState
case class EmptyFile(path: String) extends FileState
```

**Назначение**: Каждый case class представляет конкретное состояние файла.

**Особенности Scala**:

- `case class` автоматически генерирует методы `equals`, `hashCode`, `toString`, `copy` и `apply`[^9]
- Автоматически реализуется паттерн Value Object (неизменяемый объект)
- Можно использовать в pattern matching без дополнительных экстракторов
- `copy` метод позволяет создавать модифицированные копии объектов

**Состояния**:

- `Unchanged`: файл не изменился
- `Modified`: содержимое изменилось (разные хеши)
- `Deleted`: файл был удален
- `Added`: новый файл появился
- `Renamed`: файл переименован (тот же хеш, другой путь)
- `ErrorFile`: ошибка при обработке
- `EmptyFile`: пустой файл (0 байт)


### FileInfo

```scala
case class FileInfo(
  path: String,
  hash: Int,
  size: Long,
  lastModified: String
)
```

**Назначение**: Структура для хранения информации о файле в JSON-базе данных.

**Поля**:

- `path`: относительный путь к файлу
- `hash`: 32-битное хеш-значение (Int в Scala = 4 байта)
- `size`: размер файла в байтах (Long = 8 байт, до 9 экзабайт)
- `lastModified`: временная метка последнего изменения в строковом формате

***

## Алгоритм хеширования файлов

### BitStreamProcessor

```scala
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
```

**Назначение**: Обработка буфера байтов и вычисление XOR-хеша.[^1][^2]

**Особенность Scala**:

- `extends AnyVal` — это value class, который не создает дополнительных объектов в runtime. Компилятор оптимизирует его, избегая heap allocation[^4]
- Value classes в Scala позволяют добавлять методы к существующим типам без overhead

**Алгоритм построчно**:

```scala
var result = 0
```

Инициализируем результат нулем. Используется `var`, так как значение будет изменяться (в Scala `var` = mutable, `val` = immutable).

```scala
while (buffer.remaining() >= 2) {
  result ^= buffer.getShort() & 0xFFFF
}
```

**Как работает**:

1. `buffer.remaining()` возвращает количество оставшихся байт для чтения
2. Пока есть минимум 2 байта, читаем `Short` (16-битное число)
3. `buffer.getShort()` читает 2 байта и возвращает Short (-32768 до 32767)
4. `& 0xFFFF` преобразует signed Short в unsigned (0 до 65535) через битовую маску
5. `^=` — XOR assignment: `result = result ^ value`

**Почему XOR**: XOR операция обладает свойствами:

- Коммутативность: `a ^ b = b ^ a`
- Ассоциативность: `(a ^ b) ^ c = a ^ (b ^ c)`
- Любое значение XOR с самим собой дает 0: `a ^ a = 0`
- XOR с 0 не меняет значение: `a ^ 0 = a`

```scala
if (buffer.hasRemaining) {
  val lastByte = buffer.get() & 0xFF
  result ^= (lastByte << 8)
}
```

**Обработка последнего байта**:

1. Если остался 1 байт (нечетное количество)
2. Читаем его и маскируем как unsigned: `& 0xFF` (0-255)
3. Сдвигаем влево на 8 бит: `<< 8` (умножение на 256)
4. XOR с результатом

**Пример**: байт `0x5A` становится `0x5A00` и XOR'ится с результатом.

### calculateHash

```scala
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
```

**Назначение**: Вычисление хеша файла с обработкой ошибок.[^4][^1]

**Построчный разбор**:

```scala
def calculateHash(file: Path): FileOperation[Int] = {
```

Функция принимает `Path` (из Java NIO) и возвращает `FileOperation[Int]` — собственный монадический тип для обработки ошибок.

```scala
Try {
```

`Try` из `scala.util` — монада для обработки исключений. Оборачивает код, который может бросить exception, в `Success` или `Failure`[^2].

```scala
if (Files.isDirectory(file)) {
  FileError("Это каталог")
```

**Критическое исправление**: Проверяем, является ли path директорией. Это предотвращает ошибку "Это каталог" при попытке прочитать папку как файл.

```scala
} else if (!Files.exists(file) || !Files.isReadable(file)) {
  FileError(s"Файл недоступен: $file")
```

Проверка существования и прав на чтение. `s"..."` — string interpolation в Scala, `$file` вставляет значение переменной.

```scala
val size = Files.size(file)
if (size == 0) {
  FileSuccess(0)
```

Пустые файлы получают хеш = 0 без чтения. Оптимизация для избежания I/O операций.

```scala
val channel = FileChannel.open(file, StandardOpenOption.READ)
```

`FileChannel` — Java NIO класс для эффективного чтения файлов. Быстрее, чем `FileInputStream`, так как использует direct buffer и меньше системных вызовов[^4].

```scala
val buffer = ByteBuffer.allocate(math.min(size.toInt, 1024 * 1024))
```

**Буфер для чтения**:

- `ByteBuffer` — Java NIO структура для работы с байтами
- Размер: минимум из размера файла или 1MB (1048576 байт)
- `math.min` защищает от OutOfMemoryError при больших файлах
- Для файла 500KB буфер = 500KB, для 5MB буфер = 1MB

```scala
var hash = 0
```

Аккумулятор для хеша. Используем `var`, так как будем обновлять в цикле.

```scala
while (channel.read(buffer) != -1) {
```

**Цикл чтения**:

- `channel.read(buffer)` читает данные из файла в буфер
- Возвращает количество прочитанных байт или -1 при EOF (End of File)
- Буфер заполняется последовательно

```scala
buffer.flip()
```

**Критический метод**! `flip()` переключает буфер из режима записи в режим чтения:

- Устанавливает `limit = position` (сколько данных записали)
- Устанавливает `position = 0` (начало для чтения)
- Без `flip()` `remaining()` вернет 0, и данные не прочитаются

```scala
val processor = new BitStreamProcessor(buffer)
hash ^= processor.processXor
```

Создаем value class процессор и XOR'им его результат с текущим хешем. Каждый блок файла вносит вклад в итоговый хеш.[^1]

```scala
buffer.clear()
```

`clear()` готовит буфер к следующей записи:

- `position = 0`
- `limit = capacity`
- Данные НЕ удаляются, просто сбрасываются указатели

```scala
} finally {
  channel.close()
}
```

`finally` гарантирует закрытие channel даже при exception. Важно для освобождения файловых дескрипторов.

```scala
} match {
  case Success(result) => result
  case Failure(e) => FileError(s"Ошибка чтения: ${e.getMessage}")
}
```

Pattern matching на результате `Try`. Извлекаем `FileOperation` из `Success` или конвертируем exception в `FileError`.[^2]

**Почему XOR вместо SHA-256**:

- XOR работает в 10-100 раз быстрее криптографических хешей[^2][^4]
- Для обнаружения изменений файлов достаточно простого хеша
- Не нужна криптографическая стойкость (не защищаем от атак)
- Меньше нагрузка на CPU при обработке тысяч файлов

***

## Алгоритм обхода директорий

### walkDirectory

```scala
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
```

**Назначение**: Рекурсивный обход дерева директорий с пропуском служебных папок.

**Алгоритм детально**:

### Валидация входных данных

```scala
if (!Files.exists(root)) {
  println(s"❌ Директория не существует: $root")
  return List.empty
}

if (!Files.isDirectory(root)) {
  println(s"❌ Указанный путь не является директорией: $root")
  return List.empty
}
```

**Early return pattern**: Быстрый выход при невалидных данных. В Scala `return` редко используется (предпочитают выражения), но здесь оправдан для читаемости.

### Вложенная рекурсивная функция

```scala
def walkRecursive(dir: Path, depth: Int = 0): List[Path] = {
```

**Особенность Scala**: Вложенные функции (nested functions) имеют доступ к переменным внешней функции. `depth = 0` — default parameter (параметр по умолчанию).

```scala
val indent = "  " * depth
```

**String multiplication**: В Scala строки можно умножать. `"  " * 3 = "      "`. Создает отступы для визуализации глубины дерева.

```scala
import scala.jdk.CollectionConverters._
val entries = Files.list(dir).iterator().asScala.toList
```

**Java interop**:

- `Files.list(dir)` возвращает Java `Stream<Path>`
- `.iterator()` конвертирует Stream в Iterator
- `.asScala` (из CollectionConverters) оборачивает Java Iterator в Scala Iterator
- `.toList` материализует все элементы в Scala List

**Почему не stream напрямую**: Java Stream не имеет прямого эквивалента в Scala, через Iterator конверсия более предсказуема.

```scala
entries.flatMap { path =>
```

**`flatMap`** — ключевая функция:

- Применяет функцию к каждому элементу
- Функция возвращает коллекцию (List)
- Все коллекции "расплющиваются" в одну
- Пример: `List(1,2,3).flatMap(x => List(x, x*10)) = List(1,10,2,20,3,30)`

В нашем случае:

- Для файлов возвращаем `List(path)` (список из одного элемента)
- Для директорий возвращаем результат рекурсии (список всех файлов в поддереве)
- Для пропускаемых элементов возвращаем `List.empty`
- `flatMap` объединяет все это в один плоский список файлов


### Логика фильтрации

```scala
if (Files.isDirectory(path)) {
  if (fileName != OUTPUT_DIR_NAME && !Files.isSymbolicLink(path)) {
    walkRecursive(path, depth + 1)
```

**Рекурсивный спуск**:

- Если элемент — директория
- И это НЕ `disk_auditor_reports` (наша служебная папка)
- И это НЕ символическая ссылка (во избежание циклов)
- То рекурсивно обходим её с увеличенной глубиной

```scala
} else if (fileName == OUTPUT_DIR_NAME) {
  println(s"$indent⚠️ Пропускаем директорию отчетов: $fileName")
  List.empty
```

Явно пропускаем директорию с отчетами, возвращая пустой список.

```scala
} else {
  println(s"$indent⚠️ Пропускаем символическую ссылку: $fileName")
  List.empty
}
```

Символические ссылки опасны — могут создать бесконечную рекурсию.

```scala
} else {
  println(s"$indent📄 Найден файл: $fileName")
  List(path)
}
```

Файл — добавляем в результат как `List(path)`.

### Обработка ошибок

```scala
} catch {
  case e: AccessDeniedException =>
    println(s"$indent❌ Нет доступа к директории: ${dir.getFileName} - ${e.getMessage}")
    List.empty
  case e: Exception =>
    println(s"$indent❌ Ошибка при обходе директории: ${dir.getFileName} - ${e.getMessage}")
    List.empty
}
```

**Pattern matching для exceptions**:

- Специфичная обработка для `AccessDeniedException` (нет прав)
- Общая обработка для всех других исключений
- Возвращаем пустой список — пропускаем проблемную директорию, но продолжаем обход остальных

**Почему List.empty**: В Scala нет `null`, используем пустые коллекции для представления "отсутствия данных".

### Визуализация работы

Для структуры:

```
root/
├── file1.txt
├── subdir1/
│   ├── file2.txt
│   └── subdir2/
│       └── file3.txt
└── disk_auditor_reports/ (пропускается)
```

Алгоритм:

1. `walkRecursive(root, 0)` → `List(root/file1.txt) ++ walkRecursive(subdir1, 1) ++ List.empty`
2. `walkRecursive(subdir1, 1)` → `List(subdir1/file2.txt) ++ walkRecursive(subdir2, 2)`
3. `walkRecursive(subdir2, 2)` → `List(subdir2/file3.txt)`
4. Итог: `List(root/file1.txt, subdir1/file2.txt, subdir2/file3.txt)`

***

## Алгоритм отслеживания изменений

### scanFiles

```scala
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
        if (msg != "Это каталог") {
          errorList += (relativePath -> msg)
        }
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
```

**Назначение**: Сканирование всех файлов и создание snapshot текущего состояния.

**Детальный разбор**:

### Concurrent коллекции

```scala
val resultMap = TrieMap[String, FileInfo]()
val errorList = TrieMap[String, String]()
val emptyFiles = TrieMap[String, Unit]()
```

**`TrieMap`** — thread-safe hash map из Scala concurrent collections:

- Не требует явной синхронизации
- Основан на lock-free алгоритмах (без блокировок)
- Эффективен для параллельного доступа
- В данном коде используется последовательно, но готов к параллелизации

**Почему Unit**: `Map[String, Unit]` фактически работает как `Set[String]`, но с возможностью хранить дополнительные данные в будущем.

### Обработка файлов

```scala
files.zipWithIndex.foreach { case (file, index) =>
```

**`zipWithIndex`**:

- Превращает `List[Path]` в `List[(Path, Int)]`
- Каждый элемент получает индекс: `(element, 0), (element, 1), ...`
- Позволяет отслеживать прогресс

```scala
val relativePath = root.relativize(file).toString.replace('\\', '/')
```

**Относительный путь**:

- `root.relativize(file)` убирает префикс корневой директории
- `/home/user/project/file.txt` → `file.txt`
- `/home/user/project/dir/file.txt` → `dir/file.txt`
- `.replace('\\', '/')` нормализует разделители для Windows

**Зачем**: Хранение относительных путей делает базу переносимой между системами.

### Pattern matching результата

```scala
calculateHash(file) match {
  case FileSuccess(hash) =>
```

Обрабатываем успешное вычисление хеша.

```scala
val size = Files.size(file)
val lastModified = Files.getLastModifiedTime(file).toString
```

Собираем метаданные файла. `lastModified` в формате ISO-8601: `2025-09-30T22:45:00Z`.

```scala
if (size == 0) {
  emptyFiles += (relativePath -> ())
}
```

Отмечаем пустые файлы для статистики.

```scala
resultMap += (relativePath -> FileInfo(relativePath, hash, size, lastModified))
```

**Добавление в Map**: `+=` это синтаксический сахар для `resultMap.put(key, value)`.

### Прогресс-бар

```scala
val processed = index + 1
if (processed % 10 == 0 || processed == totalFilesCount) {
  val progress = (processed * 100) / totalFilesCount
  println(s"📈 Прогресс: $processed/$totalFilesCount ($progress%)")
}
```

**Логика**:

- Выводим каждые 10 файлов: `processed % 10 == 0`
- Или в конце: `processed == totalFilesCount`
- Процент: `(10 * 100) / 100 = 10%`


### Обработка ошибок

```scala
case FileError(msg) =>
  if (msg != "Это каталог") {
    errorList += (relativePath -> msg)
  }
```

**Фильтрация ошибок**: Ошибка "Это каталог" не добавляется в список, так как это нормальная ситуация (мы уже отфильтровали директории в `walkDirectory`, но double-check не помешает).

### Вывод статистики

```scala
if (errorList.nonEmpty) {
  println(s"\n⚠️ Ошибки при обработке файлов (${errorList.size}):")
  errorList.take(10).foreach { case (path, error) =>
    println(s"  - $path: $error")
  }
  if (errorList.size > 10) {
    println(s"  ... и еще ${errorList.size - 10} ошибок")
  }
}
```

**Ограничение вывода**: Показываем максимум 10 ошибок, чтобы не захламлять консоль.

```scala
resultMap.toMap
```

Конвертируем `TrieMap` в обычный immutable `Map` для возврата.

***

## Алгоритм определения переименований

### compareStates

```scala
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
```

**Назначение**: Сравнение двух состояний файловой системы и определение всех изменений.[^6][^10]

**Это самая сложная функция в коде. Разберем подробно:**

### Подготовка данных

```scala
val oldPaths = oldState.keySet
val newPaths = newState.keySet
```

Множества всех путей в старом и новом состоянии. `Set` для O(1) проверки принадлежности.

### Создание обратных индексов

```scala
val oldHashToPath = oldState.groupBy(_._2.hash).map { case (hash, entries) =>
  hash -> entries.keys.toSet
}
```

**Сложная конструкция, разберем по шагам**:

1. `oldState.groupBy(_._2.hash)`:
    - `oldState` это `Map[String, FileInfo]`
    - Итерируем по парам `(path, fileInfo)`
    - `_._2` извлекает второй элемент tuple (FileInfo)
    - `.hash` извлекает поле hash
    - `groupBy` группирует по хешу: `Map[Int, Map[String, FileInfo]]`
2. `.map { case (hash, entries) => hash -> entries.keys.toSet }`:
    - `entries` это `Map[String, FileInfo]` для данного хеша
    - `.keys` извлекает пути (Set[String])
    - Результат: `Map[Int, Set[String]]` — для каждого хеша список путей

**Пример**:

```
oldState = {
  "file1.txt" -> FileInfo(hash=123, ...),
  "file2.txt" -> FileInfo(hash=456, ...),
  "file3.txt" -> FileInfo(hash=123, ...)
}

oldHashToPath = {
  123 -> Set("file1.txt", "file3.txt"),
  456 -> Set("file2.txt")
}
```

**Зачем**: Быстро найти все файлы с определенным хешем.

### Алгоритм определения переименований

```scala
val renames = scala.collection.mutable.Set[(String, String, Int)]()
val processedOld = scala.collection.mutable.Set[String]()
val processedNew = scala.collection.mutable.Set[String]()
```

Mutable коллекции для накопления результатов. Tuple `(String, String, Int)` = (старый путь, новый путь, хеш).

```scala
oldState.foreach { case (oldPath, oldInfo) =>
```

Перебираем все файлы из старого состояния.

```scala
if (!newPaths.contains(oldPath) && oldInfo.hash != 0) {
```

**Условия для потенциального переименования**:

1. `!newPaths.contains(oldPath)` — файл исчез со своего старого места
2. `oldInfo.hash != 0` — это не пустой файл (для пустых файлов переименование не определяется)
```scala
newHashToPath.get(oldInfo.hash).foreach { paths =>
```

**Ищем файл с таким же хешем в новом состоянии**:

- `newHashToPath.get(hash)` возвращает `Option[Set[String]]`
- `.foreach` выполнится, только если нашли (`Some`)
- `paths` — множество новых путей с таким хешем

```scala
paths.foreach { newPath =>
  if (!oldPaths.contains(newPath) && !processedNew.contains(newPath)) {
    renames += ((oldPath, newPath, oldInfo.hash))
    processedOld += oldPath
    processedNew += newPath
  }
}
```

**Финальные проверки**:

- `!oldPaths.contains(newPath)` — этот путь был добавлен (его не было раньше)
- `!processedNew.contains(newPath)` — этот новый путь еще не использован для другого переименования

**Если все условия выполнены** — это переименование!

- Добавляем в `renames`
- Помечаем оба пути как обработанные

**Пример работы алгоритма**:

```
Старое состояние:
  "dir/old_name.txt" -> hash=999

Новое состояние:
  "dir/new_name.txt" -> hash=999

Алгоритм:
1. Рассматриваем "dir/old_name.txt"
2. Его нет в newPaths ✓
3. hash != 0 ✓
4. Ищем hash=999 в newHashToPath → находим "dir/new_name.txt"
5. "dir/new_name.txt" не было в oldPaths ✓
6. Еще не обработано ✓
7. Заключение: RENAMED("dir/old_name.txt", "dir/new_name.txt", 999)
```


### Определение остальных изменений

```scala
val allPaths = (oldPaths ++ newPaths).toSeq.sorted
```

Объединяем все пути из обоих состояний и сортируем. `++` — конкатенация множеств (дубликаты удаляются автоматически).

```scala
allPaths.flatMap { path =>
  if (processedOld.contains(path) || processedNew.contains(path)) {
    None
```

**Пропускаем переименованные файлы**: Если путь уже обработан как часть переименования, возвращаем `None` (будет отфильтровано `flatMap`).

```scala
(oldState.get(path), newState.get(path)) match {
```

**Tuple pattern matching**: Получаем `Option[FileInfo]` из обоих состояний и сопоставляем все комбинации.

```scala
case (Some(old), Some(neu)) if old.hash == neu.hash =>
  Some(Unchanged(path, old.hash))
```

Файл есть в обоих состояниях, хеши совпадают → **не изменился**.

```scala
case (Some(old), Some(neu)) =>
  Some(Modified(path, old.hash, neu.hash))
```

Файл есть в обоих состояниях, хеши разные → **изменен**.

```scala
case (Some(old), None) =>
  Some(Deleted(path, old.hash))
```

Файл был, теперь его нет → **удален**.

```scala
case (None, Some(neu)) =>
  Some(Added(path, neu.hash))
```

Файла не было, теперь есть → **добавлен**.

```scala
} ++ renames.map { case (oldPath, newPath, hash) =>
  Renamed(oldPath, newPath, hash)
}
```

**Объединение результатов**: К списку обычных изменений добавляем все найденные переименования.

### Почему алгоритм работает корректно

**Ключевая идея**: Переименование = исчезновение одного пути + появление другого пути с **тем же хешом**.

**Граничные случаи**:

1. **Файл скопирован**: Оба пути остаются → определится как Unchanged + Added
2. **Два файла переименованы друг в друга**: `processedNew` предотвращает конфликты, обрабатывается первый найденный
3. **Файл изменен и переименован**: Хеши не совпадут → определится как Deleted + Added (не как Renamed)
4. **Пустые файлы**: Пропускаются (`hash != 0`), так как все имеют одинаковый хеш

***

## Генерация HTML-отчетов

### DirectoryNode

```scala
case class DirectoryNode(name: String, files: List[FileState], subdirs: Map[String, DirectoryNode])
```

**Назначение**: Древовидная структура для представления файловой системы в отчете.

**Рекурсивная структура**: `subdirs` содержит другие `DirectoryNode`, формируя дерево.

### buildDirectoryTree

```scala
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
```

**Назначение**: Построение дерева директорий из плоского списка путей.

**Алгоритм рекурсивной вставки**:

```scala
def insertIntoTree(node: DirectoryNode, pathParts: List[String], state: FileState): DirectoryNode = {
```

Функция принимает текущий узел, части пути и состояние файла, возвращает обновленный узел.

```scala
pathParts match {
  case Nil => node
```

Базовый случай: путь исчерпан, возвращаем узел без изменений.

```scala
case fileName :: Nil =>
  node.copy(files = state :: node.files)
```

**Последний элемент пути (файл)**:

- `fileName :: Nil` — список из одного элемента
- `node.copy(...)` создает новый узел с измененным полем `files`
- `state :: node.files` добавляет state в начало списка (cons оператор)

**Immutability**: `copy` создает новый объект, не модифицирует исходный. Это функциональный подход Scala.

```scala
case dirName :: rest =>
  val subdir = node.subdirs.getOrElse(dirName, DirectoryNode(dirName, List.empty, Map.empty))
  val updatedSubdir = insertIntoTree(subdir, rest, state)
  node.copy(subdirs = node.subdirs + (dirName -> updatedSubdir))
```

**Рекурсивный случай (директория)**:

1. `dirName :: rest` — путь начинается с имени директории и есть еще части
2. `getOrElse` — получаем существующую поддиректорию или создаем новую
3. Рекурсивно вставляем в поддиректорию оставшийся путь (`rest`)
4. Создаем новый узел с обновленной Map поддиректорий

**Пример работы**:

```
Вставка "dir1/dir2/file.txt":

1. insertIntoTree(root, ["dir1", "dir2", "file.txt"], state)
   - Создаем/находим dir1
   - Рекурсия: insertIntoTree(dir1_node, ["dir2", "file.txt"], state)

2. insertIntoTree(dir1_node, ["dir2", "file.txt"], state)
   - Создаем/находим dir2
   - Рекурсия: insertIntoTree(dir2_node, ["file.txt"], state)

3. insertIntoTree(dir2_node, ["file.txt"], state)
   - Последний элемент → добавляем в files
   - Возвращаем dir2_node.copy(files = state :: dir2_node.files)

4. Разматываем рекурсию, обновляя узлы снизу вверх
```


### foldLeft

```scala
val root = DirectoryNode("root", List.empty, Map.empty)
results.foldLeft(root) { (tree, state) =>
```

**`foldLeft`** — левосторонняя свертка (fold):

- Начальное значение: `root` (пустое дерево)
- Функция: `(tree, state) => insertIntoTree(tree, ...)`
- Применяется к каждому элементу `results` последовательно
- Аккумулятор `tree` передается в следующую итерацию

**Визуализация**:

```
results = [state1, state2, state3]

tree0 = root
tree1 = insertIntoTree(tree0, state1)
tree2 = insertIntoTree(tree1, state2)
tree3 = insertIntoTree(tree2, state3)
результат = tree3
```


### renderDirectoryTree

```scala
def renderDirectoryTree(node: DirectoryNode, level: Int = 0): String = {
  val indent = "&nbsp;&nbsp;&nbsp;&nbsp;" * level
  val dirHeader = if (level > 0) {
    s"""<div style="margin-top: 10px; font-weight: bold; color: #333;">$indent📁 ${node.name}</div>"""
  } else {
    ""
  }

  val filesHtml = node.files.sortBy { ... }.map { state =>
    val content = state match {
      case Renamed(oldPath, newPath, _) =>
        val oldFileName = oldPath.split('/').last
        val newFileName = newPath.split('/').last
        s"$indent&nbsp;&nbsp;&nbsp;&nbsp;↻ Переименован: $oldFileName → $newFileName".withClass("renamed").colorize("purple")
      // ... другие случаи
    }
    content.wrapTag("div")
  }.mkString("\n")

  val subdirsHtml = node.subdirs.toSeq.sortBy(_._1).map { case (_, subdir) =>
    renderDirectoryTree(subdir, level + 1)
  }.mkString("\n")

  dirHeader + filesHtml + subdirsHtml
}
```

**Назначение**: Рекурсивное преобразование дерева в HTML.

**Структура**:

1. `indent` — HTML отступы (`&nbsp;` = non-breaking space)
2. `dirHeader` — заголовок директории (пропускается для корня)
3. `filesHtml` — файлы в текущей директории
4. `subdirsHtml` — рекурсивно обработанные поддиректории

**Для переименованных файлов**:

```scala
case Renamed(oldPath, newPath, _) =>
  val oldFileName = oldPath.split('/').last
  val newFileName = newPath.split('/').last
  s"$indent&nbsp;&nbsp;&nbsp;&nbsp;↻ Переименован: $oldFileName → $newFileName"
```

**Извлечение имен**:

- `split('/')` разбивает путь: `"dir/subdir/file.txt"` → `["dir", "subdir", "file.txt"]`
- `.last` берет последний элемент: `"file.txt"`
- Отображаем: `old_name.txt → new_name.txt`


### Implicit class HtmlOps

```scala
implicit class HtmlOps(s: String) {
  def wrapTag(tag: String): String = s"<$tag>$s</$tag>"
  def withClass(className: String): String = s"""<span class="$className">$s</span>"""
  def colorize(color: String): String = s"""<span style="color: $color;">$s</span>"""
}
```

**Особенность Scala**: Implicit classes добавляют методы к существующим типам.

**Как работает**:

```scala
"текст".wrapTag("div")
```

Компилятор видит, что у `String` нет метода `wrapTag`, находит implicit класс `HtmlOps` и автоматически преобразует в:

```scala
new HtmlOps("текст").wrapTag("div")
```

**Результат**: `"<div>текст</div>"`

**Chain calling**:

```scala
"файл".withClass("renamed").colorize("purple")
// → "<span style=\"color: purple;\"><span class=\"renamed\">файл</span></span>"
```


***

## Особенности Scala

### 1. Type Safety (Типобезопасность)

```scala
sealed trait FileState
case class Modified(path: String, oldHash: Int, newHash: Int) extends FileState
```

**Exhaustive matching**:

```scala
state match {
  case Unchanged(p, _) => ...
  case Modified(p, _, _) => ...
  // Компилятор предупредит, если забыли Deleted, Added и т.д.
}
```

**Отсутствие null**: Вместо `null` используются `Option[T]`, `Either[L, R]`, пустые коллекции.

### 2. Immutability (Неизменяемость)

```scala
case class FileInfo(path: String, hash: Int, size: Long, lastModified: String)
```

**Все поля `val` по умолчанию** (в case class). Нельзя изменить после создания. Изменения делаются через `copy`:

```scala
val info = FileInfo("file.txt", 123, 1000, "2025-01-01")
val updated = info.copy(hash = 456)  // новый объект
```


### 3. Pattern Matching

```scala
calculateHash(file) match {
  case FileSuccess(hash) => ...
  case FileError(msg) => ...
}
```

**Мощнее switch из Java/C**:

- Работает с типами, не только примитивами
- Деструктуризация (destructuring): `case Modified(path, old, new) => ...`
- Guard clauses: `case Some(x) if x > 10 => ...`
- Exhaustiveness checking


### 4. For-Comprehensions и Monads

```scala
sealed trait FileOperation[+A] {
  def map[B](f: A => B): FileOperation[B]
  def flatMap[B](f: A => FileOperation[B]): FileOperation[B]
}
```

**Монада** — абстракция для композиции операций. Наш `FileOperation` — простая монада для обработки ошибок.

**Можно использовать for-comprehension**:

```scala
for {
  hash <- calculateHash(file)
  info <- processHash(hash)
  result <- saveInfo(info)
} yield result
```

Компилятор преобразует в `flatMap` и `map`.

### 5. Higher-Order Functions

```scala
files.flatMap { path => ... }
results.foldLeft(root) { (tree, state) => ... }
```

**Функции — объекты первого класса**:

- Можно передавать как параметры
- Возвращать из функций
- Хранить в переменных


### 6. Implicit Conversions

```scala
import scala.jdk.CollectionConverters._
val scalaList = javaIterator.asScala.toList
```

**Java Interop**: Scala предоставляет implicit конверсии для работы с Java коллекциями, но их нужно явно импортировать (в Scala 2.13+).

### 7. String Interpolation

```scala
s"Прогресс: $processed/$totalFilesCount ($progress%)"
```

**Три вида**:

- `s"..."` — простая интерполяция
- `f"$value%.2f"` — с форматированием
- `raw"..."` — без экранирования `\n` и т.д.


### 8. Option Type

```scala
val maybeValue: Option[Int] = Some(42)
val nothing: Option[Int] = None

maybeValue.foreach(v => println(v))  // выведет 42
nothing.foreach(v => println(v))      // ничего не выведет
```

**Безопасная замена null**: Компилятор заставляет обрабатывать отсутствие значения.

### 9. Collections API

**Богатый API**:

- `map`, `flatMap`, `filter`, `fold`, `reduce`, `groupBy`, `partition`
- Lazy collections: `Iterator`, `LazyList` (Stream в Scala 2.12)
- Parallel collections: `par` (в данном коде не используется)

```scala
List(1, 2, 3).map(_ * 2).filter(_ > 3)  // List(4, 6)
```


### 10. Value Classes

```scala
class BitStreamProcessor(val buffer: ByteBuffer) extends AnyVal
```

**Zero-cost abstraction**: Компилятор удаляет обертку в runtime, вызывая методы напрямую. Нет overhead.

***

## Производительность и оптимизации

### 1. Буферизация I/O

```scala
val buffer = ByteBuffer.allocate(math.min(size.toInt, 1024 * 1024))
```

**1MB буфер** балансирует:

- Память (не загружаем весь файл)
- Скорость (меньше системных вызовов)


### 2. XOR вместо криптографических хешей

**Преимущества**:

- В 10-100 раз быстрее SHA-256[^4][^2]
- Достаточно для определения изменений
- Низкая вероятность коллизий для реальных файлов

**Недостатки**:

- Не криптографически стойкий
- Легко создать коллизии преднамеренно
- Не подходит для защиты от атак


### 3. Обратные индексы для переименований

```scala
val oldHashToPath = oldState.groupBy(_._2.hash)
```

**O(1) lookup** вместо O(n) перебора. Для 10000 файлов это критично.

### 4. Относительные пути

```scala
val relativePath = root.relativize(file).toString
```

**Меньше данных** в JSON, быстрее сравнение строк.

***

## Возможные улучшения

### 1. Параллелизация

```scala
files.par.foreach { file =>
  calculateHash(file)
}
```

Использовать `.par` для параллельной обработки на многоядерных системах.

### 2. Инкрементальное хеширование

Хешировать только измененные файлы (по `lastModified`):

```scala
if (oldInfo.lastModified == newInfo.lastModified) {
  // Файл не изменился, используем старый хеш
}
```


### 3. Streaming для больших файлов

Вместо загрузки 1MB блоков, использовать streaming:

```scala
Source.fromFile(file).getLines().foreach { line => ... }
```


### 4. Компрессия JSON

Сжимать базу данных хешей gzip'ом для экономии места.

### 5. Более стойкий хеш

Для критичных применений заменить XOR на SHA-256 или Blake3.[^5][^2][^4]

***

## Заключение

Этот скрипт демонстрирует ключевые концепции функционального программирования на Scala:

- **Immutability** — все data structures неизменяемы
- **Type safety** — sealed traits и exhaustive matching
- **Higher-order functions** — flatMap, foldLeft, groupBy
- **Pattern matching** — для control flow
- **Monadic error handling** — FileOperation вместо exceptions

Алгоритмы оптимизированы для:

- **Скорости** — XOR хеширование, буферизация, обратные индексы
- **Памяти** — streaming file reading, не загружаем все в RAM
- **Надежности** — обработка ошибок без падения программы

Код написан в чисто функциональном стиле без мутаций (кроме локальных `var` для производительности) и может быть легко расширен для параллельной обработки, более сложных алгоритмов детекции изменений или интеграции в CI/CD pipeline для мониторинга целостности критичных файлов.[^10][^1]
<span style="display:none">[^3][^7][^8]</span>

<div align="center">⁂</div>

[^1]: https://stackoverflow.com/questions/37307097/compute-file-content-hash-with-scala

[^2]: https://mojoauth.com/hashing/sha-256-in-scala/

[^3]: https://mojoauth.com/hashing/dhash-in-scala/

[^4]: https://compile7.org/hashing/how-to-use-sha-512-in-scala/

[^5]: https://github.com/kcrypt/scala-sha

[^6]: https://compile7.org/hashing/how-to-use-sha-384-in-scala/

[^7]: https://contributors.scala-lang.org/t/equality-and-hash-typeclasses/1735

[^8]: https://rosenville.com/scala-md5-sha-string-hashing/

[^9]: https://users.scala-lang.org/t/help-writing-equals-and-a-compatible-hashcode/5721

[^10]: https://www.reddit.com/r/learnprogramming/comments/hxo0no/best_hash_algorithm_for_comparing_files_w/

