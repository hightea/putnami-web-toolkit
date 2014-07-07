package fr.putnami.pwt.core.security.shared.domain;

import java.io.Serializable;

import javax.validation.constraints.NotNull;

public class SignInRequest implements Serializable {
	@NotNull
	private String username;
	@NotNull
	private String password;

	public String getUsername() {
		return username;
	}

	public void setUsername(String username) {
		this.username = username;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

}
