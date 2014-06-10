package fr.putnami.pwt.sample.web.client;

import java.util.logging.Logger;

import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.core.client.GWT;
import com.google.gwt.i18n.client.ConstantsWithLookup;
import com.google.gwt.user.client.ui.RootPanel;

import fr.putnami.pwt.core.common.client.error.ErrorManager;
import fr.putnami.pwt.core.error.client.SimpleErrorDisplayer;
import fr.putnami.pwt.core.mvp.client.MvpController;
import fr.putnami.pwt.sample.web.client.application.SampleDisplay;
import fr.putnami.pwt.sample.web.client.view.addressbook.AddressBookPlace;
import fr.putnami.pwt.sample.web.client.view.contactslist.ContactsPlace;
import fr.putnami.pwt.sample.web.shared.constant.ErrorConstants;

public class SampleWebApp implements EntryPoint {
	private static final Logger LOGGER = Logger.getLogger(SampleWebApp.class.getName());

	private final SampleDisplay applicationMenu = new SampleDisplay();

	@Override
	public void onModuleLoad() {
		RootPanel.get().add(applicationMenu);

		MvpController controller = MvpController.get();
		controller.setDisplay(applicationMenu);

		SimpleErrorDisplayer errorDisplayer = new SimpleErrorDisplayer();
		errorDisplayer.setConstants((ConstantsWithLookup) GWT.create(ErrorConstants.class));
		ErrorManager.get().setErrorDisplayer(errorDisplayer);

		controller.setDefaultPlace(ContactsPlace.INSTANCE);

		controller.registerActivity(ContactsPlace.INSTANCE);
		controller.registerActivity(AddressBookPlace.INSTANCE);

		controller.handleCurrentHistory();

	}
}
