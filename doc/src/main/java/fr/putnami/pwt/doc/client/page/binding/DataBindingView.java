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
package fr.putnami.pwt.doc.client.page.binding;

import java.util.List;

import javax.validation.constraints.NotNull;

import com.google.common.collect.Lists;
import com.google.gwt.core.client.GWT;
import com.google.gwt.i18n.client.ConstantsWithLookup;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.user.client.ui.Widget;

import fr.putnami.pwt.core.editor.client.helper.MessageHelper;
import fr.putnami.pwt.core.editor.shared.constant.ValidationConstants;
import fr.putnami.pwt.core.inject.client.annotation.InjectModel;
import fr.putnami.pwt.core.inject.client.annotation.PostConstruct;
import fr.putnami.pwt.core.model.client.model.Model;
import fr.putnami.pwt.core.widget.client.Form;
import fr.putnami.pwt.core.widget.client.InputList;
import fr.putnami.pwt.core.widget.client.OutputList;
import fr.putnami.pwt.core.widget.client.TableEditor;
import fr.putnami.pwt.core.widget.client.binder.UiBinderLocalized;
import fr.putnami.pwt.doc.client.application.Page;

public class DataBindingView extends Page {

	interface Binder extends UiBinderLocalized<Widget, DataBindingView> {
	}

	public static class Address {
		@NotNull
		public String city = "New York";
	}

	public static class Person {
		@NotNull
		public String name = "john doe";
		@NotNull
		public String email = "john@doe.com";

		public Address address = new Address();

		public Person() {
		}

		public Person(String name, String email) {
			super();
			this.name = name;
			this.email = email;
		}

	}

	public interface Constants extends ConstantsWithLookup, ValidationConstants {
		@DefaultStringValue("Name")
		String name();

		@DefaultStringValue("Name...")
		String namePlaceholder();

		@DefaultStringValue("City")
		String city();

		@DefaultStringValue("E-mail")
		String email();

		@DefaultStringValue("E-mail...")
		String emailPlaceholder();
	}

	@InjectModel
	Model<Person> personModel;

	@UiField
	Form<Person> beanForm;
	@UiField
	Form<Person> fieldsetForm;

	@UiField
	TableEditor<Person> tableEditor;
	@UiField
	OutputList<Person> outputListEditor;
	@UiField
	Form<Person> outputListItemTemplate;

	@UiField
	InputList<Person> inputListEditor;
	@UiField
	Form<Person> inputListInItemTemplate;
	@UiField
	Form<Person> inputListOutItemTemplate;

	@Override
	@PostConstruct
	public void postConstruct() {
		super.postConstruct();
		List<Person> people = Lists.newArrayList();
		people.add(new Person());
		people.add(new Person("Jane Doe", "jane@doe.com"));
		people.add(new Person("Roger Waren", "roger@waren.org"));
		people.add(new Person("Fred Stone", "irene@stone-family.org"));

		MessageHelper messageHelper = new MessageHelper((ConstantsWithLookup) GWT.create(Constants.class));

		beanForm.setMessageHelper(messageHelper);
		beanForm.initialize(personModel);
		beanForm.edit(new Person());

		fieldsetForm.setMessageHelper(messageHelper);
		fieldsetForm.initialize(personModel);
		fieldsetForm.edit(new Person());

		tableEditor.setMessageHelper(messageHelper);
		tableEditor.initialize(personModel);
		tableEditor.edit(people);

		outputListEditor.setMessageHelper(messageHelper);
		outputListEditor.initialize(personModel);
		outputListItemTemplate.setMessageHelper(messageHelper);
		outputListItemTemplate.initialize(personModel);
		outputListEditor.edit(people);

		inputListEditor.setMessageHelper(messageHelper);
		inputListEditor.initialize(personModel);
		inputListInItemTemplate.setMessageHelper(messageHelper);
		inputListInItemTemplate.initialize(personModel);
		inputListOutItemTemplate.setMessageHelper(messageHelper);
		inputListOutItemTemplate.initialize(personModel);
		inputListEditor.edit(people);

	}

	@Override
	protected UiBinderLocalized getBinder() {
		return GWT.create(Binder.class);
	}

}
