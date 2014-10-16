package fr.putnami.pwt.sample.web.client;

import com.google.gwt.core.client.GWT;
import com.google.gwt.i18n.client.ConstantsWithLookup;

import fr.putnami.pwt.core.error.client.ErrorManager;
import fr.putnami.pwt.core.error.client.widget.SimpleErrorDisplayer;
import fr.putnami.pwt.core.inject.client.Module;
import fr.putnami.pwt.core.inject.client.annotation.EntryPointHandler;
import fr.putnami.pwt.core.inject.client.annotation.MvpDescription;
import fr.putnami.pwt.core.inject.client.annotation.ThemeDescription;
import fr.putnami.pwt.sample.web.client.application.SampleDisplay;
import fr.putnami.pwt.sample.web.client.view.addressbook.AddressBookView;
import fr.putnami.pwt.sample.web.client.view.contactslist.ContactsView;
import fr.putnami.pwt.sample.web.shared.constant.ErrorConstants;

@MvpDescription(display = SampleDisplay.class, defaultPlace = AddressBookView.AddressBookPlace.class,
	activities = {
		AddressBookView.AddressBookPlace.class,
		ContactsView.ContactsPlace.class,
})
@ThemeDescription(styleSheets = "theme/sample/style/pwt-sample-web.css")
public class SampleWebApp implements Module {

	@EntryPointHandler
	void onModuleStart() {
		SimpleErrorDisplayer errorDisplayer = new SimpleErrorDisplayer();
		errorDisplayer.setConstants((ConstantsWithLookup) GWT.create(ErrorConstants.class));
		ErrorManager.get().setErrorDisplayer(errorDisplayer);
	}
}
