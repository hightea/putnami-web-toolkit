package fr.putnami.pwt.core.security.shared.exception;

import fr.putnami.pwt.core.service.shared.exception.CommandException;

public class BadAuthenticationException extends CommandException {

	public BadAuthenticationException() {
	}

	public BadAuthenticationException(String message) {
		super(message);
	}


}
