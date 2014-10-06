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
package fr.putnami.pwt.doc.client.page.binding;

import com.google.common.collect.Lists;
import com.google.gwt.i18n.client.ConstantsWithLookup;
import com.google.gwt.uibinder.client.UiField;

import java.util.List;

import javax.validation.constraints.NotNull;

import fr.putnami.pwt.core.editor.shared.constant.ValidationConstants;
import fr.putnami.pwt.core.inject.client.annotation.Initialize;
import fr.putnami.pwt.core.inject.client.annotation.PostConstruct;
import fr.putnami.pwt.core.inject.client.annotation.Templated;
import fr.putnami.pwt.core.mvp.client.ViewPlace;
import fr.putnami.pwt.core.mvp.client.annotation.ActivityDescription;
import fr.putnami.pwt.core.widget.client.Form;
import fr.putnami.pwt.core.widget.client.InputList;
import fr.putnami.pwt.core.widget.client.OutputList;
import fr.putnami.pwt.core.widget.client.TableEditor;
import fr.putnami.pwt.doc.client.application.Page;
import fr.putnami.pwt.doc.client.application.SummaryDecorator;

@Templated
public class DataBindingPage extends Page {
	@ActivityDescription(view = DataBindingPage.class, viewDecorator = SummaryDecorator.class)
	public static class DataBindingPlace extends ViewPlace {
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

	@UiField
	@Initialize(constantsClass = Constants.class)
	Form<Person> beanForm;
	@UiField
	@Initialize(constantsClass = Constants.class)
	Form<Person> fieldsetForm;

	@UiField
	@Initialize(constantsClass = Constants.class)
	TableEditor<Person> tableEditor;
	@UiField
	@Initialize(constantsClass = Constants.class)
	OutputList<Person> outputListEditor;
	@UiField
	@Initialize(constantsClass = Constants.class)
	Form<Person> outputListItemTemplate;

	@UiField
	@Initialize(constantsClass = Constants.class)
	InputList<Person> inputListEditor;
	@UiField
	@Initialize(constantsClass = Constants.class)
	Form<Person> inputListInItemTemplate;
	@UiField
	@Initialize(constantsClass = Constants.class)
	Form<Person> inputListOutItemTemplate;

	@PostConstruct
	public void postConstruct() {

		List<Person> people = Lists.newArrayList();
		people.add(new Person());
		people.add(new Person("Jane Doe", "jane@doe.com"));
		people.add(new Person("Roger Waren", "roger@waren.org"));
		people.add(new Person("Fred Stone", "irene@stone-family.org"));

		beanForm.edit(new Person());
		fieldsetForm.edit(new Person());
		tableEditor.edit(people);
		outputListEditor.edit(people);
		inputListEditor.edit(people);

	}

}
