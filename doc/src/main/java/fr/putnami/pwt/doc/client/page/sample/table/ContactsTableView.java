/**
 * This file is part of pwt-doc.
 *
 * pwt-doc is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * pwt-doc is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with pwt-doc.  If not, see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.doc.client.page.sample.table;

import java.util.List;

import com.google.common.collect.LinkedHashMultimap;
import com.google.common.collect.Lists;
import com.google.common.collect.Multimap;
import com.google.gwt.core.client.GWT;
import com.google.gwt.dom.client.Document;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.uibinder.client.UiHandler;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.gwt.user.client.ui.Widget;

import fr.putnami.pwt.core.editor.client.event.FlushSuccessEvent;
import fr.putnami.pwt.core.editor.client.helper.MessageHelper;
import fr.putnami.pwt.core.model.client.model.Model;
import fr.putnami.pwt.core.mvp.client.View;
import fr.putnami.pwt.core.widget.client.Form;
import fr.putnami.pwt.core.widget.client.Modal;
import fr.putnami.pwt.core.widget.client.TableEditor;
import fr.putnami.pwt.core.widget.client.binder.UiBinderLocalized;
import fr.putnami.pwt.core.widget.client.event.ButtonEvent;
import fr.putnami.pwt.core.widget.client.event.RowClickEvent;
import fr.putnami.pwt.core.widget.client.event.SelectionEvent;
import fr.putnami.pwt.doc.client.page.sample.SampleView;
import fr.putnami.pwt.doc.client.page.sample.constants.SampleConstants;
import fr.putnami.pwt.doc.client.page.sample.domain.Contact;
import fr.putnami.pwt.doc.client.page.sample.domain.Person;
import fr.putnami.pwt.doc.client.page.sample.service.ContactService;

public class ContactsTableView extends SampleView<ContactsTablePlace> implements View<ContactsTablePlace> {

	interface Binder extends UiBinderLocalized<Widget, ContactsTableView> {
		UiBinderLocalized<Widget, ContactsTableView> BINDER = GWT.create(Binder.class);
	}

	public interface ContactModel extends Model<Contact> {

		Model<Contact> MODEL = GWT.create(ContactModel.class);
	}

	@UiField(provided = true)
	final SampleConstants constants = GWT.create(SampleConstants.class);

	@UiField(provided = true)
	final List<Integer> weightItems = generateWeightItems();

	@UiField
	Form<Contact> contactEditor;

	@UiField
	TableEditor<Contact> contactTable;

	@UiField
	Modal modal;

	private final IsWidget sampleWidget;

	public ContactsTableView() {
		super();

		sampleWidget = Binder.BINDER.createAndBindUi(this);

		MessageHelper messageHelper = new MessageHelper(constants);

		contactTable.setMessageHelper(messageHelper);
		contactTable.initialize(ContactModel.MODEL);
		contactEditor.setMessageHelper(messageHelper);
		contactEditor.initialize(ContactModel.MODEL);

		Multimap<String, String> sources = LinkedHashMultimap.create();
		sources.put(VIEW_PANEL, "table/ContactsTableView.ui.xml");
		sources.put(VIEW_PANEL, "table/ContactsTableView.java");
		sources.put(VIEW_PANEL, "table/ContactsTablePlace.java");
		sources.put(SERVICE_PANEL, "service/ContactService.java");
		sources.put(DOMAIN_PANEL, "domain/Person.java");
		sources.put(DOMAIN_PANEL, "domain/Contact.java");
		sources.put(DOMAIN_PANEL, "domain/Address.java");
		sources.put(DOMAIN_PANEL, "domain/Gender.java");
		sources.put(DOMAIN_PANEL, "domain/Group.java");
		sources.put(CONSTANTS_PANEL, "constants/SampleConstants.java");
		addSources(sources);
	}

	@Override
	protected IsWidget getSampleWidget() {
		return sampleWidget;
	}

	@Override
	public void present(ContactsTablePlace place) {
		super.present(place);
		Document.get().setTitle("PWT - Sample - Contact table");
		contactTable.edit(Lists.<Contact> newArrayList(ContactService.get().getPeople()));
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

	@UiHandler("contactEditor")
	void onSave(FlushSuccessEvent event) {
		ContactService.get().savePerson((Contact) event.getValue());
		modal.hide();
		present(null);
	}

	private List<Integer> generateWeightItems() {
		List<Integer> result = Lists.newArrayList();
		for (int i = 30; i < 121; i++) {
			result.add(i);
		}
		return result;
	}

}
