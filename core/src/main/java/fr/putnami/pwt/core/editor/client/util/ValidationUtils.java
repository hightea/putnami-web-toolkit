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
package fr.putnami.pwt.core.editor.client.util;

import com.google.common.collect.Lists;

import java.util.Collection;

import fr.putnami.pwt.core.editor.client.EditorInput;
import fr.putnami.pwt.core.editor.client.Error;
import fr.putnami.pwt.core.editor.client.impl.SimpleError;
import fr.putnami.pwt.core.editor.client.validator.Validator;

public final class ValidationUtils {

	public static <A> Collection<Error> validate(Collection<Validator<A>> validators,
		EditorInput<A> editor, A value) {
		Collection<Error> errors = Lists.newArrayList();
		if (validators != null) {
			for (Validator<A> validator : validators) {
				Error error = validator.validate(editor, value);
				if (error != null) {
					errors.add(error);
				}
			}
		}
		return errors;
	}

	private ValidationUtils() {
	}

	public static <A> Error createError(EditorInput<A> editor, String message, A value,
		Object... paramerters) {
		return new SimpleError(editor, message, value, paramerters);
	}
}
