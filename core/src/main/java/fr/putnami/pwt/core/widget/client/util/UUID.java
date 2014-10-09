/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
 * implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
 * General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with pwt. If not,
 * see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.core.widget.client.util;

import java.util.Random;

/**
 * The Class UUID helps to generate a dummy UUID looking like RFC4122 v4.
 */
public final class UUID {
	private static final char[] CHARS = "0123456789ABCDEF".toCharArray();

	private static final Random RANDOM = new Random();

	/**
	 * Instantiates a new uuid.
	 */
	private UUID() {
	}

	/**
	 * Generate an UUID looks like RFC4122 v4.
	 * <p>
	 * <strong>Example : </strong>D8DA62A2-9844-D4F6-0120-73D61116ED45
	 * </p>
	 *
	 * @return the unique id string
	 */
	public static String uuid() {
		char[] uuid = new char[36];
		uuid[8] = uuid[13] = uuid[18] = uuid[23] = '-';
		for (int i = 0; i < 36; i++) {
			if (uuid[i] == 0) {
				uuid[i] = UUID.CHARS[UUID.randomInt(16)];
			}
		}
		return new String(uuid);
	}

	/**
	 * Random int.
	 *
	 * @param radix the radix
	 * @return the int
	 */
	private static int randomInt(int radix) {
		return Math.abs(RANDOM.nextInt() % radix);
	}
}