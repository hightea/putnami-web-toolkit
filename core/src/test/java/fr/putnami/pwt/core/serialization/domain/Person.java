package fr.putnami.pwt.core.serialization.domain;

import com.google.common.base.Objects;

import java.io.Serializable;

public class Person implements Serializable {

	private String name;
	private Gender gender;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public Gender getGender() {
		return gender;
	}

	public void setGender(Gender gender) {
		this.gender = gender;
	}

	@Override
	public int hashCode() {
		return Objects.hashCode(name, gender);
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof Person) {
			Person other = (Person) obj;
			return Objects.equal(name, other.name)
				&& Objects.equal(gender, other.gender);
		}
		return false;
	}

	@Override
	public String toString() {
		return Objects
			.toStringHelper(this)
			.add("name", name)
			.toString();
	}

}
