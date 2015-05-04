package tuenti

import java.security.MessageDigest
import Utils._

/**
 * @author Eduardo
 */
object Problem10 extends App {

  def dictionary = fromFile("english.txt")

  def md5(s: String) = {
    UtilsHex.convert(MessageDigest.getInstance("MD5").digest(s.getBytes))
  }

  val original1 = "xapp_name%3Dhsl%26db_name%3Dhsl%26db_user%3Dhsl%26db_passwd%3DCBXN2a%40-Q_aV%407D_%26db_host%3Dlocalhost"
  val original2 = "xapp_name%3dhsl%26db_name%3dhsl%26db_user%3dhsl%26db_passwd%3dcbxn2a%40-q_av%407d_%26db_host%3dlocalhost"
  val originalHashCode = "2e3408c61118229d8e3c9b380a7ab2bb"

  val target = "xapp_name%3dhsl%26db_name%3dgotilio%26db_user%3dgotilio%26db_passwd%3dgotilio%26db_host%3ddb4free.net"

  println(originalHashCode)
  println(md5(original1))
  println(md5(original2))
  println(md5(target))

}
