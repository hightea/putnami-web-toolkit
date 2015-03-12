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
package fr.putnami.pwt.core.serialization.domain;

import com.google.common.base.Objects;

import java.io.Serializable;
import java.util.Date;

public class BeanPublicFields implements Serializable {
	public boolean booleanVal;
	public byte byteVal;
	public char charVal;
	public double doubleVal;
	public float floatVal;
	public int intVal;
	public long longVal;
	public short shortVal;

	public String stringObject;
	public Boolean booleanObject;
	public Byte byteObject;
	public Character charObject;
	public Double doubleObject;
	public Float floatObject;
	public Integer intObject;
	public Long longObject;
	public Short shortObject;
	public Date dateObject;

	@Override
	public int hashCode() {
		return Objects.hashCode(
			stringObject,
			booleanVal,
			byteVal,
			charVal,
			doubleVal,
			floatVal,
			intVal,
			longVal,
			shortVal,
			booleanObject,
			byteObject,
			charObject,
			doubleObject,
			floatObject,
			intObject,
			longObject,
			shortObject,
			dateObject
			);
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof BeanPublicFields) {
			BeanPublicFields other = (BeanPublicFields) obj;
			return Objects.equal(stringObject, other.stringObject)
				&& Objects.equal(booleanVal, other.booleanVal)
				&& Objects.equal(byteVal, other.byteVal)
				&& Objects.equal(charVal, other.charVal)
				&& Objects.equal(doubleVal, other.doubleVal)
				&& Objects.equal(floatVal, other.floatVal)
				&& Objects.equal(intVal, other.intVal)
				&& Objects.equal(longVal, other.longVal)
				&& Objects.equal(shortVal, other.shortVal)
				&& Objects.equal(booleanObject, other.booleanObject)
				&& Objects.equal(byteObject, other.byteObject)
				&& Objects.equal(charObject, other.charObject)
				&& Objects.equal(doubleObject, other.doubleObject)
				&& Objects.equal(floatObject, other.floatObject)
				&& Objects.equal(intObject, other.intObject)
				&& Objects.equal(longObject, other.longObject)
				&& Objects.equal(shortObject, other.shortObject)
				&& Objects.equal(dateObject, other.dateObject);
		}
		return false;
	}

	@Override
	public String toString() {
		return Objects.toStringHelper(this)
			.add("stringObject", stringObject)
			.add("booleanVal", booleanVal)
			.add("intVal", intVal)
			.toString();
	}
}