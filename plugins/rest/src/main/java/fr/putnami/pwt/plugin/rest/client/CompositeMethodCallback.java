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

import com.google.gwt.core.client.GWT;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import java.util.List;

public class CompositeMethodCallback<T> implements MethodCallback<T> {

	private final boolean quiet;
	private final List<MethodCallback<T>> callbackList;

	public CompositeMethodCallback(boolean quiet, List<MethodCallback<T>> callbackList) {
		this.quiet = quiet;
		this.callbackList = callbackList;
	}

	private void fireResponseEvent(Method method) {
		if (!quiet) {
			// FireEvent
		}
	}

	@Override
	public void onSuccess(Method method, T response) {
		fireResponseEvent(method);
		for (MethodCallback<T> methodCallback : callbackList) {
			methodCallback.onSuccess(method, response);
		}
	}

	@Override
	public void onFailure(Method method, Throwable exception) {
		fireResponseEvent(method);
		boolean caught = false;
		for (MethodCallback<T> methodCallback : callbackList) {
			try {
				methodCallback.onFailure(method, exception);
				caught = true;
			} catch (RuntimeException e) {
				// Exception not handled.
				continue;
			}
		}
		if (!caught) {
			GWT.reportUncaughtException(exception);
		}
	}
}
