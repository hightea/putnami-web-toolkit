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
package fr.putnami.pwt.doc.client.page.sample.table;

import com.google.common.collect.LinkedHashMultimap;
import com.google.common.collect.Lists;
import com.google.common.collect.Multimap;
import com.google.gwt.dom.client.Document;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.uibinder.client.UiHandler;
import com.google.gwt.user.client.ui.Composite;

import java.util.List;

import fr.putnami.pwt.core.editor.client.event.FlushSuccessEvent;
import fr.putnami.pwt.core.inject.client.annotation.Initialize;
import fr.putnami.pwt.core.inject.client.annotation.PresentHandler;
import fr.putnami.pwt.core.inject.client.annotation.Templated;
import fr.putnami.pwt.core.mvp.client.View;
import fr.putnami.pwt.core.mvp.client.ViewPlace;
import fr.putnami.pwt.core.mvp.client.annotation.ActivityDescription;
import fr.putnami.pwt.core.widget.client.Form;
import fr.putnami.pwt.core.widget.client.Modal;
import fr.putnami.pwt.core.widget.client.TableEditor;
import fr.putnami.pwt.core.widget.client.event.ButtonEvent;
import fr.putnami.pwt.core.widget.client.event.RowClickEvent;
import fr.putnami.pwt.core.widget.client.event.SelectionEvent;
import fr.putnami.pwt.doc.client.page.sample.decorator.HasSources;
import fr.putnami.pwt.doc.client.page.sample.decorator.SampleDecorator;
import fr.putnami.pwt.doc.shared.page.sample.constants.SampleConstants;
import fr.putnami.pwt.doc.shared.page.sample.domain.Contact;
import fr.putnami.pwt.doc.shared.page.sample.service.ContactService;

@Templated
public class ContactsTablePage extends Composite implements View, HasSources {
	@ActivityDescription(view = ContactsTablePage.class, viewDecorator = SampleDecorator.class)
	public static class ContactsTablePlace extends ViewPlace {
	}

	@UiField(provided = true)
	final List<Integer> weightItems = this.generateWeightItems();

	@UiField
	@Initialize(constantsClass = SampleConstants.class)
	Form<Contact> contactEditor;

	@UiField
	@Initialize(constantsClass = SampleConstants.class)
	TableEditor<Contact> contactTable;

	@UiField
	Modal modal;

	private final Multimap<String, String> sources = LinkedHashMultimap.create();

	public ContactsTablePage() {
		super();

		this.sources.put(HasSources.VIEW_PANEL, "table/ContactsTablePage.ui.xml");
		this.sources.put(HasSources.VIEW_PANEL, "table/ContactsTablePage.java");
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
	void present(ContactsTablePlace place) {
		Document.get().setTitle("PWT - Sample - Contact table");
		this.contactTable.edit(Lists.<Contact> newArrayList(ContactService.get().getPeople()));
	}

	@UiHandler("clickMeBoutton")
	void onClickMeBouttonEvent(ButtonEvent event) {
		this.contactEditor.edit(new Contact());
		this.modal.toggleVisibility();
	}

	@UiHandler("contactTable")
	void onRowClik(RowClickEvent event) {
	}

	@UiHandler("contactTableSelecter")
	void onPersonSelected(SelectionEvent event) {
	}

	@UiHandler("selectContactBoutton")
	void onSelectContactEvent(ButtonEvent event) {
		Contact collab = event.getValue();
		this.contactEditor.edit(collab);
		this.modal.toggleVisibility();
	}

	@UiHandler("cancelButton")
	void onCancelButton(ButtonEvent event) {
		this.modal.hide();
	}

	@UiHandler("contactEditor")
	void onSave(FlushSuccessEvent event) {
		ContactService.get().savePerson((Contact) event.getValue());
		this.modal.hide();
		this.present(null);
	}

	private List<Integer> generateWeightItems() {
		List<Integer> result = Lists.newArrayList();
		for (int i = 30; i < 121; i++) {
			result.add(i);
		}
		return result;
	}

}
