@file:OptIn(ExperimentalSerializationApi::class)

import kotlinx.coroutines.*
import kotlinx.coroutines.Dispatchers.IO
import kotlinx.serialization.*
import kotlinx.serialization.json.*
import java.io.File
import java.nio.file.Paths
import java.security.MessageDigest
import java.time.Instant

@Serializable
private data class PassFile(
    var magic: String? = null,
    var hash: String = "",
    var blockedAt: String? = null,
    var attempts: Int = 0
)

private const val FILE_NAME = "pass.json"
private const val LOG_NAME = "pass.log"
private const val MAGIC = "INFOBEZ_TASK"
private const val MAX_ATTEMPTS = 3

class PasswordPolicy {
    var minLength: Int = 6
    var requireDigit: Boolean = true
    var requireLatin: Boolean = true
    var requireCyrillic: Boolean = true
    var requireUpper: Boolean = true
    var requireLower: Boolean = true
    var requireSpecial: Boolean = false
    var allowedSpecial = setOf('!', '@', '#', '$', '%', '^', '&', '*', '-', '_', '+', '=')

    fun validate(pwd: String): List<String> {
        val errors = mutableListOf<String>()
        if (pwd.length < minLength) errors += "Длина < $minLength символов."
        if (requireDigit && pwd.none { it.isDigit() }) errors += "Нет цифры."
        if (requireLatin && pwd.none { it in 'a'..'z' || it in 'A'..'Z' }) errors += "Нет латинской буквы."
        if (requireCyrillic && pwd.none { it in '\u0410'..'\u044F' || it == 'ё' || it == 'Ё' }) errors += "Нет кириллической буквы."
        if (requireUpper && pwd.none { it.isUpperCase() }) errors += "Нет заглавной буквы."
        if (requireLower && pwd.none { it.isLowerCase() }) errors += "Нет строчной буквы."
        if (requireSpecial && pwd.none { it in allowedSpecial }) errors += "Нет спецсимвола (${allowedSpecial.joinToString("")})."
        return errors
    }
}

/* ---------- 3️⃣ Утилиты ---------- */

private fun sha256Base62(input: String): String {
    val md = MessageDigest.getInstance("SHA-256")
    val digest = md.digest(input.toByteArray(Charsets.UTF_8))

    val charset = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    var num = java.math.BigInteger(1, digest)
    val sb = StringBuilder()
    while (num > java.math.BigInteger.ZERO) {
        val divRem = num.divideAndRemainder(java.math.BigInteger.valueOf(62))
        sb.append(charset[divRem[1].toInt()])
        num = divRem[0]
    }
    return sb.toString().reversed()
}

suspend fun readLineWithTimeout(prompt: String, timeoutMs: Long = 30_000L): String? =
    withContext(IO) {
        print(prompt); System.out.flush()
        withTimeoutOrNull(timeoutMs) {
            System.`in`.bufferedReader().readLine()
        }
    }

private fun log(message: String) {
    val ts = Instant.now().toString()
    File(LOG_NAME).appendText("[$ts] $message\n")
}

private suspend fun firstRun(file: File, policy: PasswordPolicy) {
    println("=== Первый запуск ===")
    while (true) {
        val pwd = readLineWithTimeout("Задайте новый пароль: ") ?: ""
        val errors = policy.validate(pwd)
        if (errors.isEmpty()) {
            val hash = sha256Base62(pwd)
            val data = PassFile(hash = hash, blockedAt = null, attempts = 0)
            writeJson(file, data)
            log("Пароль установлен, hash=$hash")
            println("Пароль успешно сохранён.")
            break
        } else {
            println("Пароль не подходит:")
            errors.forEach { println(" • $it") }
        }
    }
}

private suspend fun verify(data: PassFile, file: File, policy: PasswordPolicy) {
    repeat(MAX_ATTEMPTS) { attemptIdx ->
        val attemptNo = attemptIdx + 1
        val pwd = readLineWithTimeout("Пароль ($attemptNo/$MAX_ATTEMPTS): ") ?: ""
        if (sha256Base62(pwd) == data.hash) {
            log("Успешный вход с $attemptNo попытки")
            println("Доступ разрешён.")
            data.attempts = 0
            writeJson(file, data)
            return
        } else {
            println("Неверно.")
            data.attempts++
            writeJson(file, data)
            log("Неудачная попытка #$attemptNo")
        }
    }

    data.blockedAt = Instant.now().toString()
    writeJson(file, data)
    log("Блокировка после $MAX_ATTEMPTS неудач")
    println("Слишком много неверных попыток. Программа заблокирована.")
}

private val json = Json { prettyPrint = true; ignoreUnknownKeys = true }

private suspend fun writeJson(file: File, data: PassFile) = withContext(IO) {
    file.writeText(json.encodeToString(data))
}

private suspend fun readJson(file: File): PassFile? = withContext(IO) {
    if (!file.exists()) return@withContext null
    try {
        json.decodeFromString<PassFile>(file.readText())
    } catch (e: Exception) {
        null
    }
}

fun main() = runBlocking {
    val passFile = Paths.get(FILE_NAME).toFile()
    val policy = PasswordPolicy().apply {
        minLength = 8
        requireSpecial = true
    }

    if (!passFile.exists()) {
        println("Файл $FILE_NAME отсутствует – приложение завершено.")
        log("Запуск без файла → завершение")
        return@runBlocking
    }

    val data = readJson(passFile)
    if (data == null) {
        println("Не удалось прочитать $FILE_NAME. Возможно, повреждён JSON.")
        log("Ошибка чтения JSON")
        return@runBlocking
    }

    data.blockedAt?.let {
        println("Программа заблокирована с $it")
        log("Попытка запуска после блокировки")
        return@runBlocking
    }

    if (data.magic.equals(MAGIC)  && data.hash.isBlank()) {
        firstRun(passFile, policy)
        return@runBlocking
    }

    verify(data, passFile, policy)
}