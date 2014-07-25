package fr.putnami.pwt.core.widget.client.util;
public class UUID {
	private static final char[] CHARS = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz".toCharArray();

	public static String uuid(int len) {
		return uuid(len, CHARS.length);
	}

	public static String uuid(int len, int radix) {
		if (radix > CHARS.length) {
			throw new IllegalArgumentException();
		}
		char[] uuid = new char[len];
		// Compact form
		for (int i = 0; i < len; i++) {
			uuid[i] = CHARS[(int)(Math.random()*radix)];
		}
		return new String(uuid);
	}

	public static String uuid() {
		char[] uuid = new char[36];
		int r;

		uuid[8] = uuid[13] = uuid[18] = uuid[23] = '-';
		uuid[14] = '4';

		for (int i = 0; i < 36; i++) {
			if (uuid[i] == 0) {
				r = (int) (Math.random()*16);
				uuid[i] = CHARS[i == 19 ? r & 0x3 | 0x8 : r & 0xf];
			}
		}
		return new String(uuid);
	}
}