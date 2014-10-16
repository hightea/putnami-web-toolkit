package fr.putnami.pwt.sample.web.client.view.contactslist;

import com.google.common.collect.Lists;
import com.google.gwt.core.client.GWT;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.uibinder.client.UiHandler;
import com.google.gwt.user.client.ui.Composite;

import java.util.List;

import fr.putnami.pwt.core.inject.client.annotation.Initialize;
import fr.putnami.pwt.core.inject.client.annotation.InjectService;
import fr.putnami.pwt.core.inject.client.annotation.PresentHandler;
import fr.putnami.pwt.core.inject.client.annotation.Templated;
import fr.putnami.pwt.core.mvp.client.View;
import fr.putnami.pwt.core.mvp.client.ViewPlace;
import fr.putnami.pwt.core.mvp.client.annotation.ActivityDescription;
import fr.putnami.pwt.core.service.client.annotation.AsyncHandler;
import fr.putnami.pwt.core.widget.client.Form;
import fr.putnami.pwt.core.widget.client.Modal;
import fr.putnami.pwt.core.widget.client.TableEditor;
import fr.putnami.pwt.core.widget.client.event.ButtonEvent;
import fr.putnami.pwt.core.widget.client.event.RowClickEvent;
import fr.putnami.pwt.core.widget.client.event.SelectionEvent;
import fr.putnami.pwt.sample.web.shared.constant.AddressConstants;
import fr.putnami.pwt.sample.web.shared.constant.GenderConstants;
import fr.putnami.pwt.sample.web.shared.constant.PersonConstants;
import fr.putnami.pwt.sample.web.shared.constant.SampleCommonConstants;
import fr.putnami.pwt.sample.web.shared.domain.Contact;
import fr.putnami.pwt.sample.web.shared.domain.Person;
import fr.putnami.pwt.sample.web.shared.service.ContactService;

@Templated
public class ContactsView extends Composite implements View {

	@ActivityDescription(view = ContactsView.class)
	public static class ContactsPlace extends ViewPlace {
	}

	interface Constants extends SampleCommonConstants, PersonConstants, AddressConstants,
		GenderConstants {

		@DefaultStringValue("Edit a contact !")
		String newPersonTitle();
	}


	@UiField(provided = true)
	final Constants constants = GWT.create(Constants.class);

	@UiField(provided = true)
	final List<Integer> weightItems = generateWeightItems();

	@InjectService
	ContactService contactService;

	@UiField
	@Initialize(constantsClass = Constants.class)
	Form<Contact> contactEditor;

	@UiField
	@Initialize(constantsClass = Constants.class)
	TableEditor<Contact> contactTable;

	@UiField
	Modal modal;

	@PresentHandler
	public void present() {
		contactService.getPeople();
	}

	@UiHandler("clickMeBoutton")
	void onClickMeBouttonEvent(ButtonEvent event) {
		contactEditor.edit(new Contact());
		modal.toggleVisibility();
	}

	@UiHandler("contactTable")
	void onRowClik(RowClickEvent event) {
		GWT.log("" + event.<Person> getValue().getName());
	}

	@UiHandler("contactTableSelecter")
	void onPersonSelected(SelectionEvent event) {
		GWT.log("yop");
	}

	@UiHandler("selectContactBoutton")
	void onSelectContactEvent(ButtonEvent event) {
		Contact collab = event.getValue();
		contactEditor.edit(collab);
		modal.toggleVisibility();
	}

	@UiHandler("cancelButton")
	void onCancelButton(ButtonEvent event) {
		modal.hide();
	}

	@UiHandler("saveButton")
	void onSave(ButtonEvent event) {
		Contact flushed = contactEditor.flush();
		if (!contactEditor.hasError()) {
			contactService.savePerson(flushed);
			modal.hide();
		}
	}

	@AsyncHandler
	void onGetPeople(List<Contact> result) {
		contactTable.edit(Lists.<Contact> newArrayList(result));
	}

	@AsyncHandler
	void onSavePerson(Contact result) {
		present();
	}

	private List<Integer> generateWeightItems() {
		List<Integer> result = Lists.newArrayList();
		for (int i = 30; i < 121; i++) {
			result.add(i);
		}
		return result;
	}

}
