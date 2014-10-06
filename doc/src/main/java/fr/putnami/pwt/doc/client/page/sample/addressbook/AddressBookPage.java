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
		this.sources.put(HasSources.VIEW_PANEL, "addressbook/AddressBookPage.ui.xml");
		this.sources.put(HasSources.VIEW_PANEL, "addressbook/AddressBookPage.java");
		this.sources.put(HasSources.SERVICE_PANEL, "service/ContactService.java");
		this.sources.put(HasSources.DOMAIN_PANEL, "domain/Person.java");
		this.sources.put(HasSources.DOMAIN_PANEL, "domain/Contact.java");
		this.sources.put(HasSources.DOMAIN_PANEL, "domain/Address.java");
		this.sources.put(HasSources.DOMAIN_PANEL, "domain/Gender.java");
		this.sources.put(HasSources.DOMAIN_PANEL, "domain/Group.java");
		this.sources.put(HasSources.CONSTANTS_PANEL, "constants/SampleConstants.java");
	}

	@Override
	public Multimap<String, String> getSourcesMap() {
		return this.sources;
	}

	@PresentHandler
	void presentAddressBook(AddressBookPlace place) {
		Document.get().setTitle("PWT - Sample - Address book");

		List<Group> groups = ContactService.get().getGroups();
		Collection<String> groupsItems = Lists.newArrayList();
		for (int i = 1; i < groups.size(); i++) {
			groupsItems.add(groups.get(i).getName());
		}
		this.groupSelect.setItems(groupsItems);
		this.groupsList.edit(groups);
		this.displayGroup(groups.get(0));
	}

	@UiHandler("newContactButton")
	void onNewContact(ButtonEvent event) {
		this.editContact(new Contact());
	}

	@UiHandler("editContactButton")
	void onEditContact(ButtonEvent event) {
		this.editContact(this.selected);
	}

	@UiHandler("saveContactButton")
	void onSaveContact(ButtonEvent event) {
		Contact contactToSave = this.contactDetails.flush();
		if (!this.contactDetails.hasError()) {
			ContactService.get().savePerson(contactToSave);
			this.presentAddressBook(null);
		}
	}

	@UiHandler("cancelContactButton")
	void onCancelContact(ButtonEvent event) {
		this.viewContact(this.selected);
	}

	@UiHandler("searchResetButton")
	void onResetSearch(ButtonEvent event) {
		this.searchBox.edit(null);
		this.displayList(this.displayedGroup.getMembers());
	}

	@UiHandler("searchBox")
	void onSearchBox(KeyPressEvent event) {
		final InputText source = (InputText) event.getSource();
		Scheduler.get().scheduleDeferred(new ScheduledCommand() {

			@Override
			public void execute() {
				String query = source.flush();
				if (query == null || query.length() == 0) {
					AddressBookPage.this.displayList(AddressBookPage.this.displayedList);
				} else {
					final String queryToCompare = query.toLowerCase().trim();
					Iterable<Contact> filteredIteable =
							Iterables.filter(AddressBookPage.this.displayedList, new Predicate<Contact>() {

								@Override
								public boolean apply(Contact contact) {
									return contact.getName() != null
											&& contact.getName().toLowerCase().contains(queryToCompare);
								}
							});
					AddressBookPage.this.displayList(Lists.newArrayList(filteredIteable));
				}
			}
		});
	}

	@UiHandler("selectGroupAnchor")
	void onClickGroupAnchor(ClickEvent event) {
		Anchor<Group> anchor = (Anchor<Group>) event.getSource();
		this.displayGroup(anchor.getValue());
	}

	@UiHandler("openContactAnchor")
	void onClickContactAnchor(ClickEvent event) {
		Anchor<Contact> anchor = (Anchor<Contact>) event.getSource();
		this.selectContact(anchor.getValue());
	}

	@UiHandler("contactDetails")
	void onSaveContact(DirtyEvent event) {
		this.saveContactButton.setDisabled(false);
	}

	private void displayGroup(Group group) {
		this.displayedGroup = group;
		this.displayList(group.getMembers());
	}

	private void displayList(List<Contact> contact) {
		this.displayedList = contact;
		this.contactsList.edit(Lists.<Contact> newArrayList(contact));
		if (this.displayedList.size() > 0) {
			this.viewContact(this.displayedList.get(0));
		}
	}

	private void selectContact(Contact contact) {
		if (this.selected != contact) {
			this.viewContact(contact);
		}
	}

	private void viewContact(Contact collaborator) {
		this.selected = collaborator;
		this.contactDetails.setReadonly(true);
		this.contactDetails.edit(collaborator);

		this.editContactButton.setDisabled(false);

		this.saveContactButton.setDisabled(true);
		this.cancelContactButton.setDisabled(true);

		this.editNameRow.setVisible(false);
		this.viewNameRow.setVisible(true);
	}

	private void editContact(Contact collaborator) {
		this.contactDetails.setReadonly(false);
		this.contactDetails.edit(collaborator);

		this.editContactButton.setDisabled(true);

		this.saveContactButton.setDisabled(false);
		this.cancelContactButton.setDisabled(false);

		this.editNameRow.setVisible(true);
		this.viewNameRow.setVisible(false);
	}
}
