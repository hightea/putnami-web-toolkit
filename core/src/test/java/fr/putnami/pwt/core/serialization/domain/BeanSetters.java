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

public class BeanSetters implements Serializable {
	private boolean booleanVal;
	private byte byteVal;
	private char charVal;
	private double doubleVal;
	private float floatVal;
	private int intVal;
	private long longVal;
	private short shortVal;

	private String stringObject;
	private Boolean booleanObject;
	private Byte byteObject;
	private Character charObject;
	private Double doubleObject;
	private Float floatObject;
	private Integer intObject;
	private Long longObject;
	private Short shortObject;
	private Date dateObject;

	public boolean isBooleanVal() {
		return booleanVal;
	}

	public void setBooleanVal(boolean booleanVal) {
		this.booleanVal = booleanVal;
	}

	public byte getByteVal() {
		return byteVal;
	}

	public void setByteVal(byte byteVal) {
		this.byteVal = byteVal;
	}

	public char getCharVal() {
		return charVal;
	}

	public void setCharVal(char charVal) {
		this.charVal = charVal;
	}

	public double getDoubleVal() {
		return doubleVal;
	}

	public void setDoubleVal(double doubleVal) {
		this.doubleVal = doubleVal;
	}

	public float getFloatVal() {
		return floatVal;
	}

	public void setFloatVal(float floatVal) {
		this.floatVal = floatVal;
	}

	public int getIntVal() {
		return intVal;
	}

	public void setIntVal(int intVal) {
		this.intVal = intVal;
	}

	public long getLongVal() {
		return longVal;
	}

	public void setLongVal(long longVal) {
		this.longVal = longVal;
	}

	public short getShortVal() {
		return shortVal;
	}

	public void setShortVal(short shortVal) {
		this.shortVal = shortVal;
	}

	public String getStringObject() {
		return stringObject;
	}

	public void setStringObject(String stringObject) {
		this.stringObject = stringObject;
	}

	public Boolean getBooleanObject() {
		return booleanObject;
	}

	public void setBooleanObject(Boolean booleanObject) {
		this.booleanObject = booleanObject;
	}

	public Byte getByteObject() {
		return byteObject;
	}

	public void setByteObject(Byte byteObject) {
		this.byteObject = byteObject;
	}

	public Character getCharObject() {
		return charObject;
	}

	public void setCharObject(Character charObject) {
		this.charObject = charObject;
	}

	public Double getDoubleObject() {
		return doubleObject;
	}

	public void setDoubleObject(Double doubleObject) {
		this.doubleObject = doubleObject;
	}

	public Float getFloatObject() {
		return floatObject;
	}

	public void setFloatObject(Float floatObject) {
		this.floatObject = floatObject;
	}

	public Integer getIntObject() {
		return intObject;
	}

	public void setIntObject(Integer intObject) {
		this.intObject = intObject;
	}

	public Long getLongObject() {
		return longObject;
	}

	public void setLongObject(Long longObject) {
		this.longObject = longObject;
	}

	public Short getShortObject() {
		return shortObject;
	}

	public void setShortObject(Short shortObject) {
		this.shortObject = shortObject;
	}

	public Date getDateObject() {
		return dateObject;
	}

	public void setDateObject(Date dateObject) {
		this.dateObject = dateObject;
	}

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
		if (obj instanceof BeanSetters) {
			BeanSetters other = (BeanSetters) obj;
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