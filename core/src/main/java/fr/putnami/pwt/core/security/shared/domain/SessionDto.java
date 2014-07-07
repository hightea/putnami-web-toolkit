package fr.putnami.pwt.core.security.shared.domain;

import java.io.Serializable;
import java.util.List;

import com.google.common.collect.Lists;

public class SessionDto implements Serializable {

	private String username;
	private String name;
	private String firstname;
	private String tenant;

	private List<String> roles = Lists.newArrayList();

	public String getUsername() {
		return username;
	}

	public void setUsername(String username) {
		this.username = username;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getFirstname() {
		return firstname;
	}

	public void setFirstname(String firstname) {
		this.firstname = firstname;
	}

	public String getTenant() {
		return tenant;
	}

	public void setTenant(String tenant) {
		this.tenant = tenant;
	}

	public List<String> getRoles() {
		return roles;
	}

	public void setRoles(List<String> roles) {
		this.roles = roles;
	}

}
