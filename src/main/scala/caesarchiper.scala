object caesarchiper extends App {

  val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

  //encryption function
  def encrypt(c:Char,key:Int,a:String):Char=c match{

    case x if x.isUpper => a((a.indexOf(c)+key)%a.length)
    case x if x.isLower => a((a.indexOf(c.toUpper)+key)%a.length).toLower
    case _ => ' '
  }

  //decryption function
  def decrypt(c:Char,key:Int,a:String):Char=c match{

    case x if x.isUpper => a((a.indexOf(c)-key+26)%a.length)
    case x if x.isLower => a((a.indexOf(c.toUpper)-key+26)%a.length).toLower
    case _ => ' '
  }

  val cipher=(alg:(Char,Int,String)=>Char,s:String,key:Int,a:String)=> s.map(alg(_,key,a))

  //USER INPUT
  print("Enter the word :")
  val user=scala.io.StdIn.readLine()

  print("Enter key to shift :")
  val key=scala.io.StdIn.readInt()

  //Print Encryption
  print("Encryption:")
  val x = cipher(encrypt,user,key,alphabet)
  println(x)

  //Print Decryption
  print("Decryption :")
  val y=cipher(decrypt,x,key,alphabet)
  println(y)

}
