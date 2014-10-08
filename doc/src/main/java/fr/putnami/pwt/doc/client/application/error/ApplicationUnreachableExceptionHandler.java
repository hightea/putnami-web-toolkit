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
package fr.putnami.pwt.doc.client.application.error;

import com.google.gwt.core.client.GWT;
import com.google.gwt.i18n.client.ConstantsWithLookup;

import fr.putnami.pwt.core.error.client.AbstractErrorHandler;
import fr.putnami.pwt.core.error.client.ErrorHandler;
import fr.putnami.pwt.core.mvp.client.exception.ApplicationUnreachableException;

public class ApplicationUnreachableExceptionHandler extends AbstractErrorHandler {

	private SingleErrorDisplay errorDisplay = new SingleErrorDisplay();

	interface Constants extends ConstantsWithLookup {

		@DefaultStringValue("Wrong application version")
		String applicationUnreachableTitle();

		@DefaultStringValue("Your version of the application seems to be out of date. Reload this page to get the latest application version.")
		String applicationUnreachableMessage();
	}

	private Constants constants = GWT.create(Constants.class);

	@Override
	public boolean handle(Throwable error) {
		if (!(error instanceof ApplicationUnreachableException)) {
			return false;
		}
		this.errorDisplay.show(this.constants.applicationUnreachableTitle(),
			this.constants.applicationUnreachableMessage());
		return true;
	}

	@Override
	public int getPriority() {
		return ErrorHandler.HIGHER_PRIORITY;
	}
}
