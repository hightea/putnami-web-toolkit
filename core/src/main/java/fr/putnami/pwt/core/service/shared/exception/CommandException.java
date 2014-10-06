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
package fr.putnami.pwt.core.service.shared.exception;

public class CommandException extends RuntimeException {

	private static final long serialVersionUID = 9179960545785348143L;

	private String causeClassName;
	private String causeSimpleClassName;
	private String causeMessage;
	private String causeStackTrace;

	public CommandException() {
		super();
	}

	public CommandException(String message) {
		super(message);
	}

	public CommandException(String message, Throwable cause) {
		super(message, cause);
		if (cause != null) {
			this.causeClassName = cause.getClass().getName();
			this.causeSimpleClassName = cause.getClass().getSimpleName();
			this.causeMessage = cause.getMessage();
		}
	}

	public String getCauseClassName() {
		return this.causeClassName;
	}

	public void setCauseClassName(String causeClassName) {
		this.causeClassName = causeClassName;
	}

	public String getCauseSimpleClassName() {
		return this.causeSimpleClassName;
	}

	public void setCauseSimpleClassName(String causeSimpleClassName) {
		this.causeSimpleClassName = causeSimpleClassName;
	}

	public String getCauseMessage() {
		return this.causeMessage;
	}

	public void setCauseMessage(String causeMessage) {
		this.causeMessage = causeMessage;
	}

	public String getCauseStackTrace() {
		return this.causeStackTrace;
	}

	public void setCauseStackTrace(String causeStackTrace) {
		this.causeStackTrace = causeStackTrace;
	}

}
