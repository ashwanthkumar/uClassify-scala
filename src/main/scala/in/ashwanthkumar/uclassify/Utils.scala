package in.ashwanthkumar.uclassify

import com.ning.http.util.Base64

object Utils {

  def base64Encode(str: String) = Base64.encode(str.getBytes)

  def base64Decode(str: String) = new String(Base64.decode(str))
}
