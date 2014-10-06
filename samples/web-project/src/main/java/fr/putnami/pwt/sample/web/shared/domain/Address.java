package fr.putnami.pwt.sample.web.shared.domain;

import com.google.common.base.Objects;
import com.google.common.base.Objects.ToStringHelper;

import java.io.Serializable;

public class Address implements Serializable {

	public String street;
	public String postCode;
	public String city;

	public String getStreet() {
		return this.street;
	}

	public void setStreet(String street) {
		this.street = street;
	}

	public String getPostCode() {
		return this.postCode;
	}

	public void setPostCode(String postCode) {
		this.postCode = postCode;
	}

	public String getCity() {
		return this.city;
	}

	public void setCity(String city) {
		this.city = city;
	}

	@Override
	public String toString() {
		ToStringHelper helper = Objects.toStringHelper(this);
		helper.add("postCode", this.postCode);
		helper.add("city", this.city);
		return helper.toString();
	}

}
