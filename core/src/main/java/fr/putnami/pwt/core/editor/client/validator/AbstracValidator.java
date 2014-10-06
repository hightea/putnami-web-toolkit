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
package fr.putnami.pwt.core.editor.client.validator;

import fr.putnami.pwt.core.editor.client.Editor;
import fr.putnami.pwt.core.editor.client.Error;
import fr.putnami.pwt.core.editor.client.impl.SimpleError;

public abstract class AbstracValidator<T> implements Validator<T> {

	private static final Object[] EMPTY_PARAMETERS = new Object[] {};

	private String message;

	public AbstracValidator(String message) {
		super();
		this.message = message;
	}

	public String getMessage() {
		return this.message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public String buildMessage(Object value) {
		return this.message;
	}

	@Override
	public Error validate(Editor editor, T value) {
		if (!this.isValid(value)) {
			SimpleError error = new SimpleError(editor, this.message, value, this.getParrameters());
			return error;
		}
		return null;
	}

	protected Object[] getParrameters() {
		return AbstracValidator.EMPTY_PARAMETERS;
	}

	protected abstract boolean isValid(T value);
}
