/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with pwt.  If not, see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.core.model.client.model;

public final class Primitives {

	private Primitives() {
	}

	public static Boolean castToBoolean(Object value) {
		return (Boolean) value;
	}

	public static Byte castToByte(Object value) {
		return (Byte) value;
	}

	public static Character castToCharacter(Object value) {
		return (Character) value;
	}

	public static Double castToDouble(Object value) {
		return (Double) value;
	}

	public static Float castToFloat(Object value) {
		return (Float) value;
	}

	public static Float castToObject(Object value) {
		return (Float) value;
	}

	public static Integer castToInteger(Object value) {
		return (Integer) value;
	}

	public static Long castToLong(Object value) {
		return (Long) value;
	}

	public static Short castToShort(Object value) {
		return (Short) value;
	}

	public static Void castToVoid(Object value) {
		return null;
	}

	public static boolean asPrimitive(Boolean value) {
		return value == null ? false : value.booleanValue();
	}

	public static byte asPrimitive(Byte value) {
		return value == null ? 0 : value.byteValue();
	}

	public static char asPrimitive(Character value) {
		return value == null ? 0 : value.charValue();
	}

	public static double asPrimitive(Double value) {
		return value == null ? 0 : value.doubleValue();
	}

	public static float asPrimitive(Float value) {
		return value == null ? 0 : value.floatValue();
	}

	public static int asPrimitive(Integer value) {
		return value == null ? 0 : value.intValue();
	}

	public static long asPrimitive(Long value) {
		return value == null ? 0 : value.longValue();
	}

	public static short asPrimitive(Short value) {
		return value == null ? 0 : value.shortValue();
	}

	public static void asPrimitive(Void value) {
		return;
	}

}
