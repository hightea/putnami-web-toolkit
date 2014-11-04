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
package fr.putnami.pwt.plugin.rest.client;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

public class MethodCallbackAdapter<T> implements MethodCallback<T> {

	@Override
	public void onSuccess(Method method, T response) {
		// Shall be override
	}

	@Override
	public void onFailure(Method method, Throwable caught) {
		if (caught != null) {
			if (caught instanceof RuntimeException) {
				throw (RuntimeException) caught;
			}
			throw new RuntimeException(caught);
		}
	}

}
