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
package fr.putnami.pwt.core.widget.client.assist;

import fr.putnami.pwt.core.widget.shared.assist.Oracle;

public class OracleWrapper<T> implements Oracle<T> {
	private Oracle<T> delegate;

		@Override
	public void request(Request request, Callback<T> callback) {
		delegate.request(request, callback);
		}

	public Oracle<T> getDelegate() {
		return delegate;
		}

	public void setDelagate(Oracle<T> delegate) {
		this.delegate = delegate;
		}
	}