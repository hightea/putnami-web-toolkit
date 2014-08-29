package fr.putnami.pwt.tutorial.client.contact;

import java.util.Arrays;
import java.util.List;

import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.uibinder.client.UiHandler;
import com.google.gwt.user.client.ui.Composite;

import fr.putnami.pwt.core.editor.client.event.FlushSuccessEvent;
import fr.putnami.pwt.core.inject.client.annotation.Initialize;
import fr.putnami.pwt.core.inject.client.annotation.InjectService;
import fr.putnami.pwt.core.inject.client.annotation.PresentHandler;
import fr.putnami.pwt.core.inject.client.annotation.Templated;
import fr.putnami.pwt.core.mvp.client.View;
import fr.putnami.pwt.core.service.client.annotation.AsyncHandler;
import fr.putnami.pwt.core.widget.client.Form;
import fr.putnami.pwt.tutorial.shared.constants.ContactConstants;
import fr.putnami.pwt.tutorial.shared.domain.Contact;
import fr.putnami.pwt.tutorial.shared.service.ContactService;

@Templated
public class ContactView extends Composite implements View {

	@InjectService
	ContactService service;

	@UiField(provided = true)
	final List<Integer> noteItems = Arrays.asList(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
	@UiField(provided = true)
	final List<String> subjectItems = Arrays.asList("About this tutorial", "About PWT", "About Putnami Team", "Other");

	@UiField
	@Initialize(constantsClass = ContactConstants.class)
	Form<Contact> contactEditor;

	@PresentHandler
	void present(ContactPlace place) {
		contactEditor.edit(new Contact()); // Init the form with all the default values
	}

	@UiHandler("contactEditor")
	void onContactSubmit(FlushSuccessEvent event) {
		service.sendContact((Contact) event.getValue());
	}

	@AsyncHandler
	void onSendContact(Void result) {
		contactEditor.edit(new Contact());
	}
}
