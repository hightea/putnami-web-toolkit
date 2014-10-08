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
package fr.putnami.pwt.doc.client.page.components;

import com.google.common.collect.Lists;
import com.google.gwt.i18n.client.ConstantsWithLookup;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.user.client.ui.Composite;

import java.util.List;

import fr.putnami.pwt.core.inject.client.annotation.Initialize;
import fr.putnami.pwt.core.inject.client.annotation.InjectResource;
import fr.putnami.pwt.core.inject.client.annotation.PostConstruct;
import fr.putnami.pwt.core.inject.client.annotation.Templated;
import fr.putnami.pwt.core.mvp.client.View;
import fr.putnami.pwt.core.widget.client.TableEditor;

@Templated
public class TablesView extends Composite implements View {

	public static enum Gender {
			MALE,
			FEMALE,
			UNKNOWN
	}

	public static class Address {
		public String city;

		public Address() {
		}

		public Address(String city) {
			this.city = city;
		}
	}

	public static class Bean {
		public Gender gender;
		public String name;
		public String email;
		public Address address;

		public Bean() {
		}

		public Bean(Gender gender, String name, String email, String city) {
			this.gender = gender;
			this.name = name;
			this.email = email;
			this.address = new Address(city);
		}
	}

	interface Constants extends ConstantsWithLookup {

		@DefaultStringValue("City")
		String city();

		@DefaultStringValue("Mr.")
		String genderMaleEnum();

		@DefaultStringValue("Mrs.")
		String genderFemaleEnum();

		@DefaultStringValue("Other")
		String genderUnknownEnum();

		@DefaultStringValue("Email")
		String email();

		@DefaultStringValue("Gender")
		String gender();

		@DefaultStringValue("Name")
		String name();
	}

	@UiField
	@Initialize(constantsClass = Constants.class)
	TableEditor<Bean> firstTable;
	@UiField
	@Initialize(constantsClass = Constants.class)
	TableEditor<Bean> secondTable;

	@InjectResource
	Constants constants;

	@PostConstruct
	public void postConstruct() {
		this.firstTable.edit(this.getBeans());
		this.secondTable.edit(this.getBeans());
	}

	private List<Bean> getBeans() {
		List<Bean> result = Lists.newArrayList();
		result.add(new Bean(Gender.MALE, "John Doe", "john.doe@doe.com", "New York"));
		result.add(new Bean(Gender.FEMALE, "Jane Doe", "jane.doe@doe.com", "New York"));
		result.add(new Bean(Gender.MALE, "Roger Waren", "roger.waren@waren.org", "Chicago"));
		result.add(new Bean(Gender.FEMALE, "Irene Waren", "irene.waren@waren.org", "Chicago"));
		result.add(new Bean(Gender.MALE, "Fred Stone", "fredstone@stone-family.com", "Springfield"));
		result.add(
			new Bean(Gender.FEMALE, "Amber Stone", "amber.stone@stone-family.com", "Springfield"));
		return result;
	}

}
