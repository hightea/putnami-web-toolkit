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
package fr.putnami.pwt.core.security.shared.exception;

import com.google.gwt.place.shared.Place;

public class SecurityException extends RuntimeException {

	/**
	 *
	 */
	private static final long serialVersionUID = -3817870661062610109L;
	private Place fallback;
	private String message;

	public SecurityException() {
		this(null, null);
	}

	public SecurityException(String message) {
		this(message, null);
	}

	public SecurityException(String message, Place fallback) {
		super(message);
		this.message = message;
		this.fallback = fallback;
	}

	public Place getFallback() {
		return this.fallback;
	}

	@Override
	public String getMessage() {
		return this.message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public void setFallback(Place fallback) {
		this.fallback = fallback;
	}

}
