object CaesarCipher {
  // Encryption function
  def encrypt(plainText: String, shift: Int): String = {
    val encryptedText = plainText.map { char =>
      if (char.isLetter) {
        val base = if (char.isUpper) 'A' else 'a'
        val shiftedChar = (char - base + shift) % 26
        (base + shiftedChar).toChar
      } else {
        char
      }
    }
    encryptedText
  }

  // Decryption function
  def decrypt(encryptedText: String, shift: Int): String = {
    encrypt(encryptedText, 26 - shift) // Decryption is equivalent to encryption with a negative shift
  }
}

object CipherFunction {
  def cipher(input: String, shift: Int, operation: String => (String, Int) => String): String = {
    operation(input)(shift)
  }
}


object Main extends App {
  val plainText = "Hello, World!"
  val shift = 3

  val encryptedText = CipherFunction.cipher(plainText, shift, CaesarCipher.encrypt)
  println(s"Encrypted Text: $encryptedText")

  val decryptedText = CipherFunction.cipher(encryptedText, shift, CaesarCipher.decrypt)
  println(s"Decrypted Text: $decryptedText")
}
