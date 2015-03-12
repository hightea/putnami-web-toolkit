package fr.putnami.pwt.core.serialization.domain;

import com.google.common.base.Objects;

import java.util.List;

public class Manager extends Person {

	private List<Person> staff;

	public List<Person> getStaff() {
		return staff;
	}

	public void setStaff(List<Person> staff) {
		this.staff = staff;
	}

	@Override
	public int hashCode() {
		return Objects.hashCode(getName(), getGender(), staff);
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof Manager) {
			Manager other = (Manager) obj;
			return super.equals(obj)
				&& Objects.equal(staff, other.staff);
		}
		return false;
	}

}
