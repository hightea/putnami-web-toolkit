package fr.putnami.pwt.core.security.shared.exception;

import fr.putnami.pwt.core.service.shared.exception.CommandException;

public class UnauthorizedException extends CommandException {

	public UnauthorizedException() {
	}

	public UnauthorizedException(String message) {
		super(message);
	}

}
