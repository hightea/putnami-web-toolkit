package fr.putnami.pwt.core.security.shared.exception;

import fr.putnami.pwt.core.service.shared.exception.CommandException;

public class SignFailledException extends CommandException {

	public SignFailledException() {
	}

	public SignFailledException(String message) {
		super(message);
	}


}
