object caesar_cipher {
  def encrypt(plaintext: String, shift: Int): String = {
    val encryptedChars = plaintext.map { c =>
      if (c.isLetter) {
        val base = if (c.isUpper) 'A' else 'a'
        val shiftedChar = (c - base + shift) % 26
        (base + (if (shiftedChar < 0) shiftedChar + 26 else shiftedChar)).toChar
      } else {
        c
      }
    }
    encryptedChars.mkString
  }

  def decrypt(ciphertext: String, shift: Int): String = encrypt(ciphertext, -shift)

  def cipher(text: String, shift: Int, processFunction: (String, Int) => String): String = {
    processFunction(text, shift)
  }

  def main(args: Array[String]): Unit = {
    val plaintext = "Hello, World!"
    val shift = 3

    val encryptedText = cipher(plaintext, shift, encrypt)
    println(s"Encrypted: $encryptedText")

    val decryptedText = cipher(encryptedText, shift, decrypt)
    println(s"Decrypted: $decryptedText")
  }
}



