package fr.putnami.pwt.sample.web.client.view.addressbook;

import com.google.common.base.Predicate;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.gwt.core.client.Scheduler;
import com.google.gwt.core.client.Scheduler.ScheduledCommand;
import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.KeyPressEvent;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.uibinder.client.UiHandler;
import com.google.gwt.user.client.ui.Composite;

import java.util.Collection;
import java.util.List;

import fr.putnami.pwt.core.editor.client.event.DirtyEvent;
import fr.putnami.pwt.core.inject.client.annotation.Initialize;
import fr.putnami.pwt.core.inject.client.annotation.InjectService;
import fr.putnami.pwt.core.inject.client.annotation.PresentHandler;
import fr.putnami.pwt.core.inject.client.annotation.Templated;
import fr.putnami.pwt.core.mvp.client.View;
import fr.putnami.pwt.core.mvp.client.ViewPlace;
import fr.putnami.pwt.core.mvp.client.annotation.ActivityDescription;
import fr.putnami.pwt.core.service.client.annotation.AsyncHandler;
import fr.putnami.pwt.core.widget.client.Anchor;
import fr.putnami.pwt.core.widget.client.Button;
import fr.putnami.pwt.core.widget.client.Form;
import fr.putnami.pwt.core.widget.client.GridRow;
import fr.putnami.pwt.core.widget.client.InputMultiSelect;
import fr.putnami.pwt.core.widget.client.InputText;
import fr.putnami.pwt.core.widget.client.OutputList;
import fr.putnami.pwt.core.widget.client.event.ButtonEvent;
import fr.putnami.pwt.sample.web.shared.constant.AddressConstants;
import fr.putnami.pwt.sample.web.shared.constant.ContactConstants;
import fr.putnami.pwt.sample.web.shared.constant.GenderConstants;
import fr.putnami.pwt.sample.web.shared.constant.SampleCommonConstants;
import fr.putnami.pwt.sample.web.shared.domain.Contact;
import fr.putnami.pwt.sample.web.shared.domain.Group;
import fr.putnami.pwt.sample.web.shared.service.ContactService;

@Templated
public class AddressBookView extends Composite implements View {

	@ActivityDescription(view = AddressBookView.class)
	public static class AddressBookPlace extends ViewPlace {
	}

	interface Constants extends SampleCommonConstants, ContactConstants, AddressConstants,
		GenderConstants {
	}

	@InjectService
	ContactService contactService;


	@UiField
	InputText searchBox;

	@UiField
	@Initialize(constantsClass = Constants.class)
	OutputList<Group> groupsList;

	@UiField
	@Initialize(constantsClass = Constants.class)
	OutputList<Contact> contactsList;
	@UiField
	@Initialize(constantsClass = Constants.class)
	Form<Contact> contactDetails;
	@UiField
	InputMultiSelect<String> groupSelect;
	@UiField
	GridRow editNameRow;
	@UiField
	GridRow viewNameRow;

	@UiField
	Button<Contact> newContactButton;
	@UiField
	Button<Contact> editContactButton;
	@UiField
	Button<Contact> cancelContactButton;
	@UiField
	Button<Contact> saveContactButton;

	private Contact selected = null;
	private Group displayedGroup;
	private List<Contact> displayedList;

	@PresentHandler
	public void present() {
		contactService.getGroups();
	}

	@UiHandler("newContactButton")
	void onNewContact(ButtonEvent event) {
		editContact(new Contact());
	}

	@UiHandler("editContactButton")
	void onEditContact(ButtonEvent event) {
		editContact(selected);
	}

	@UiHandler("saveContactButton")
	void onSaveContact(ButtonEvent event) {
		Contact contactToSave = contactDetails.flush();
		if (!contactDetails.hasError()) {
			contactService.savePerson(contactToSave);
		}
	}

	@UiHandler("cancelContactButton")
	void onCancelContact(ButtonEvent event) {
		viewContact(selected);
	}

	@UiHandler("contactDetails")
	void onSaveContact(DirtyEvent event) {
		saveContactButton.setDisabled(false);
	}

	@UiHandler("searchResetButton")
	void onResetSearch(ButtonEvent event) {
		searchBox.edit(null);
		displayList(displayedGroup.getMembers());
	}

	@UiHandler("searchBox")
	void onSearchBox(KeyPressEvent event) {
		final InputText source = (InputText) event.getSource();
		Scheduler.get().scheduleDeferred(new ScheduledCommand() {

			@Override
			public void execute() {
				String query = source.flush();
				if (query == null || query.length() == 0) {
					displayList(displayedList);
				} else {
					final String queryToCompare = query.toLowerCase().trim();
					Iterable<Contact> filteredIteable = Iterables.filter(displayedList, new Predicate<Contact>() {

						@Override
						public boolean apply(Contact contact) {
							return contact.getName() != null && contact.getName().toLowerCase().contains(queryToCompare);
						}
					});
					displayList(Lists.newArrayList(filteredIteable));
				}
			}
		});
	}

	@UiHandler("selectGroupAnchor")
	void onClickGroupAnchor(ClickEvent event) {
		Anchor<Group> anchor = (Anchor<Group>) event.getSource();
		displayGroup(anchor.getValue());
	}

	@UiHandler("openContactAnchor")
	void onClickContactAnchor(ClickEvent event) {
		Anchor<Contact> anchor = (Anchor<Contact>) event.getSource();
		selectContact(anchor.getValue());
	}

	@AsyncHandler
	void onGetGroups(List<Group> result) {
		Collection<String> groupsItems = Lists.newArrayList();
		for (int i = 1; i < result.size(); i++) {
			groupsItems.add(result.get(i).getName());
		}
		groupSelect.setItems(groupsItems);
		groupsList.edit(result);
		displayGroup(result.get(0));
	}

	@AsyncHandler
	void onSavePerson(Contact result) {
		present();
	}

	private void displayGroup(Group group) {
		this.displayedGroup = group;
		displayList(group.getMembers());
	}

	private void displayList(List<Contact> contact) {
		this.displayedList = contact;
		contactsList.edit(Lists.<Contact> newArrayList(contact));
		if (displayedList.size() > 0) {
			viewContact(displayedList.get(0));
		}
	}

	private void selectContact(Contact contact) {
		if (this.selected != contact) {
			viewContact(contact);
		}
	}

	private void viewContact(Contact collaborator) {
		selected = collaborator;
		contactDetails.setReadonly(true);
		contactDetails.edit(collaborator);

		editContactButton.setDisabled(false);

		cancelContactButton.setDisabled(true);
		saveContactButton.setDisabled(true);

		editNameRow.setVisible(false);
		viewNameRow.setVisible(true);
	}

	private void editContact(Contact collaborator) {
		contactDetails.setReadonly(false);
		contactDetails.edit(collaborator);

		editContactButton.setDisabled(true);

		cancelContactButton.setDisabled(false);
		// saveContactButton.setDisabled(false);

		editNameRow.setVisible(true);
		viewNameRow.setVisible(false);
	}
}
