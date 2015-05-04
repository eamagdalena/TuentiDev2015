package tuenti;

public class UtilsHex {

	public static String convert (byte[] byteData) {
	
	 //convert the byte to hex format method 1
    StringBuffer sb = new StringBuffer();
    for (int i = 0; i < byteData.length; i++) {
     sb.append(Integer.toString((byteData[i] & 0xff) + 0x100, 16).substring(1));
    }
	
    return sb.toString();
}
    
}
