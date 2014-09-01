package fr.putnami.pwt.tutorial.client.contact;

import java.util.Arrays;
import java.util.List;

import com.google.gwt.core.client.GWT;
import com.google.gwt.i18n.client.ConstantsWithLookup;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.uibinder.client.UiHandler;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.Widget;

import fr.putnami.pwt.core.editor.client.event.FlushSuccessEvent;
import fr.putnami.pwt.core.editor.client.helper.MessageHelper;
import fr.putnami.pwt.core.model.client.model.Model;
import fr.putnami.pwt.core.mvp.client.View;
import fr.putnami.pwt.core.service.client.ServiceProxy;
import fr.putnami.pwt.core.service.client.annotation.AsyncHandler;
import fr.putnami.pwt.core.widget.client.Form;
import fr.putnami.pwt.core.widget.client.binder.UiBinderLocalized;
import fr.putnami.pwt.tutorial.shared.constants.ContactConstants;
import fr.putnami.pwt.tutorial.shared.domain.Contact;
import fr.putnami.pwt.tutorial.shared.service.ContactService;

public class ContactView extends Composite implements View<ContactPlace> {

	interface Binder extends UiBinderLocalized<Widget, ContactView> {
		Binder BINDER = GWT.create(Binder.class);
	}

	interface ContactModel extends Model<Contact> {
		Model<Contact> MODEL = GWT.create(ContactModel.class);
	}

	interface ContactRemoteService extends ServiceProxy<ContactView, ContactService>, ContactService {
		ContactRemoteService SERVICE = GWT.create(ContactRemoteService.class);
	}

	@UiField
	Form<Contact> contactEditor;

	@UiField(provided = true)
	List<Integer> noteItems = Arrays.asList(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10);

	@UiField(provided = true)
	List<String> subjectItems = Arrays.asList("About this tutorial", "About PWT", "About Putnami Team", "Other");

	public ContactView() {
		initWidget(Binder.BINDER.createAndBindUi(this));

		MessageHelper messageHelper = new MessageHelper((ConstantsWithLookup) GWT.create(ContactConstants.class));
		contactEditor.setMessageHelper(messageHelper);

		contactEditor.initialize(ContactModel.MODEL);
		contactEditor.edit(new Contact()); // Init the form with all the default values

		ContactRemoteService.SERVICE.bindService(this);
	}

	@Override
	public void present(ContactPlace place) {
		// Do Nothing
	}

	@UiHandler("contactEditor")
	public void onContactSubmit(FlushSuccessEvent event) {
		ContactRemoteService.SERVICE.sendContact((Contact) event.getValue());
	}

	@AsyncHandler
	public void onSendContact(Void result) {
		contactEditor.edit(new Contact());
	}
}
