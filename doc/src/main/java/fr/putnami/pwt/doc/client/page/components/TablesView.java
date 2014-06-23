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
package fr.putnami.pwt.doc.client.page.components;

import java.util.List;

import com.google.common.collect.Lists;
import com.google.gwt.core.client.GWT;
import com.google.gwt.i18n.client.ConstantsWithLookup;
import com.google.gwt.uibinder.client.UiConstructor;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.Widget;

import fr.putnami.pwt.core.editor.client.helper.MessageHelper;
import fr.putnami.pwt.core.model.client.model.Model;
import fr.putnami.pwt.core.widget.client.NavSpy;
import fr.putnami.pwt.core.widget.client.TableEditor;
import fr.putnami.pwt.core.widget.client.binder.UiBinderLocalized;

public class TablesView extends Composite {

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

	public interface BeanModel extends Model<Bean> {

		Model<Bean> MODEL = GWT.create(BeanModel.class);
	}

	interface Binder extends UiBinderLocalized<Widget, TablesView> {
		Binder BINDER = GWT.create(Binder.class);
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

	@UiField(provided = true)
	final NavSpy tableOfContent;

	@UiField
	TableEditor<Bean> firstTable;
	@UiField
	TableEditor<Bean> secondTable;

	Constants constants = GWT.create(Constants.class);

	@UiConstructor
	public TablesView(NavSpy navSpy) {
		super();

		this.tableOfContent = navSpy;

		initWidget(Binder.BINDER.createAndBindUi(this));

		firstTable.initialize(BeanModel.MODEL);
		firstTable.edit(getBeans());

		secondTable.setMessageHelper(new MessageHelper(constants));
		secondTable.initialize(BeanModel.MODEL);
		secondTable.edit(getBeans());
	}

	private List<Bean> getBeans() {
		List<Bean> result = Lists.newArrayList();
		result.add(new Bean(Gender.MALE, "John Doe", "john.doe@doe.com", "New York"));
		result.add(new Bean(Gender.FEMALE, "Jane Doe", "jane.doe@doe.com", "New York"));
		result.add(new Bean(Gender.MALE, "Roger Waren", "roger.waren@waren.org", "Chicago"));
		result.add(new Bean(Gender.FEMALE, "Irene Waren", "irene.waren@waren.org", "Chicago"));
		result.add(new Bean(Gender.MALE, "Fred Stone", "fredstone@stone-family.com", "Springfield"));
		result.add(new Bean(Gender.FEMALE, "Amber Stone", "amber.stone@stone-family.com", "Springfield"));
		return result;
	}

}
