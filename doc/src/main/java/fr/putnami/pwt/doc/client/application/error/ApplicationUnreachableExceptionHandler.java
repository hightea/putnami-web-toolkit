package fr.putnami.pwt.doc.client.application.error;

import com.google.gwt.core.client.GWT;
import com.google.gwt.i18n.client.ConstantsWithLookup;

import fr.putnami.pwt.core.error.client.AbstractErrorHandler;
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
		errorDisplay.show(constants.applicationUnreachableTitle(), constants.applicationUnreachableMessage());
		return true;
	}

	@Override
	public int getPriority() {
		return HIGHER_PRIORITY;
	}
}
