/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with pwt.  If not, see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.doc.client.page.sample.addressbook;

import com.google.common.base.Predicate;
import com.google.common.collect.Iterables;
import com.google.common.collect.LinkedHashMultimap;
import com.google.common.collect.Lists;
import com.google.common.collect.Multimap;
import com.google.gwt.core.client.Scheduler;
import com.google.gwt.core.client.Scheduler.ScheduledCommand;
import com.google.gwt.dom.client.Document;
import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.KeyPressEvent;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.uibinder.client.UiHandler;
import com.google.gwt.user.client.ui.Composite;

import java.util.Collection;
import java.util.List;

import fr.putnami.pwt.core.editor.client.event.DirtyEvent;
import fr.putnami.pwt.core.inject.client.annotation.Initialize;
import fr.putnami.pwt.core.inject.client.annotation.PresentHandler;
import fr.putnami.pwt.core.inject.client.annotation.Templated;
import fr.putnami.pwt.core.mvp.client.View;
import fr.putnami.pwt.core.mvp.client.ViewPlace;
import fr.putnami.pwt.core.mvp.client.annotation.ActivityDescription;
import fr.putnami.pwt.core.widget.client.Anchor;
import fr.putnami.pwt.core.widget.client.Button;
import fr.putnami.pwt.core.widget.client.Form;
import fr.putnami.pwt.core.widget.client.GridRow;
import fr.putnami.pwt.core.widget.client.InputMultiSelect;
import fr.putnami.pwt.core.widget.client.InputText;
import fr.putnami.pwt.core.widget.client.OutputList;
import fr.putnami.pwt.core.widget.client.event.ButtonEvent;
import fr.putnami.pwt.doc.client.page.sample.decorator.HasSources;
import fr.putnami.pwt.doc.client.page.sample.decorator.SampleDecorator;
import fr.putnami.pwt.doc.shared.page.sample.constants.SampleConstants;
import fr.putnami.pwt.doc.shared.page.sample.domain.Contact;
import fr.putnami.pwt.doc.shared.page.sample.domain.Group;
import fr.putnami.pwt.doc.shared.page.sample.service.ContactService;

@Templated
public class AddressBookPage extends Composite implements View, HasSources {
	@ActivityDescription(view = AddressBookPage.class, viewDecorator = SampleDecorator.class)
	public static class AddressBookPlace extends ViewPlace {
	}

	@UiField
	InputText searchBox;

	@UiField
	@Initialize(constantsClass = SampleConstants.class)
	OutputList<Group> groupsList;

	@UiField
	@Initialize(constantsClass = SampleConstants.class)
	OutputList<Contact> contactsList;
	@UiField
	@Initialize(constantsClass = SampleConstants.class)
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

	private final Multimap<String, String> sources = LinkedHashMultimap.create();

	public AddressBookPage() {
		sources.put(VIEW_PANEL, "addressbook/AddressBookPage.ui.xml");
		sources.put(VIEW_PANEL, "addressbook/AddressBookPage.java");
		sources.put(SERVICE_PANEL, "service/ContactService.java");
		sources.put(DOMAIN_PANEL, "domain/Person.java");
		sources.put(DOMAIN_PANEL, "domain/Contact.java");
		sources.put(DOMAIN_PANEL, "domain/Address.java");
		sources.put(DOMAIN_PANEL, "domain/Gender.java");
		sources.put(DOMAIN_PANEL, "domain/Group.java");
		sources.put(CONSTANTS_PANEL, "constants/SampleConstants.java");
	}

	@Override
	public Multimap<String, String> getSourcesMap() {
		return sources;
	}

	@PresentHandler
	void presentAddressBook(AddressBookPlace place) {
		Document.get().setTitle("PWT - Sample - Address book");

		List<Group> groups = ContactService.get().getGroups();
		Collection<String> groupsItems = Lists.newArrayList();
		for (int i = 1; i < groups.size(); i++) {
			groupsItems.add(groups.get(i).getName());
		}
		groupSelect.setItems(groupsItems);
		groupsList.edit(groups);
		displayGroup(groups.get(0));
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
			ContactService.get().savePerson(contactToSave);
			presentAddressBook(null);
		}
	}

	@UiHandler("cancelContactButton")
	void onCancelContact(ButtonEvent event) {
		viewContact(selected);
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
				}
				else {
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

	@UiHandler("contactDetails")
	void onSaveContact(DirtyEvent event) {
		saveContactButton.setDisabled(false);
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

		saveContactButton.setDisabled(true);
		cancelContactButton.setDisabled(true);

		editNameRow.setVisible(false);
		viewNameRow.setVisible(true);
	}

	private void editContact(Contact collaborator) {
		contactDetails.setReadonly(false);
		contactDetails.edit(collaborator);

		editContactButton.setDisabled(true);

		saveContactButton.setDisabled(false);
		cancelContactButton.setDisabled(false);

		editNameRow.setVisible(true);
		viewNameRow.setVisible(false);
	}
}
