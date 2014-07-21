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
package fr.putnami.pwt.doc.client.page.components;

import java.util.Date;
import java.util.List;

import com.google.common.collect.Lists;
import com.google.gwt.core.client.GWT;
import com.google.gwt.i18n.client.ConstantsWithLookup;
import com.google.gwt.uibinder.client.UiConstructor;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.uibinder.client.UiHandler;
import com.google.gwt.user.client.Random;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.Widget;

import fr.putnami.pwt.core.editor.client.helper.MessageHelper;
import fr.putnami.pwt.core.editor.shared.constant.ValidationConstants;
import fr.putnami.pwt.core.model.client.model.Model;
import fr.putnami.pwt.core.widget.client.Fieldset;
import fr.putnami.pwt.core.widget.client.Form;
import fr.putnami.pwt.core.widget.client.NavSpy;
import fr.putnami.pwt.core.widget.client.binder.UiBinderLocalized;
import fr.putnami.pwt.core.widget.client.event.ButtonEvent;
import fr.putnami.pwt.core.widget.client.helper.DateParser;

public class FormsView extends Composite {

	public enum Gender {
		MALE,
		FEMALE,
		UNKNOWN
	}

	public static class Bean {
		public Gender gender = Gender.MALE;
		public String name = "John Doe";
		public String state = "Michigan";
		public String email = "john.doe@gmail.com";
		public String password = "secret";
		public boolean rememberMe = false;
		public boolean major = true;
		public boolean notMajor = false;
		public int age = Random.nextInt(100);
		public double height = Random.nextInt(1000000) / 100D;
		public Date birthdate = new DateParser("dd/MM/yyyy").parseOrNull("02/03/1985");
		public String remarks = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Fusce lobortis elementum vestibulum. Aliquam luctus semper congue. Fusce placerat tempus lectus, et pulvinar elit aliquam eget. Suspendisse placerat vitae risus vitae sagittis. Suspendisse dignissim orci urna, in aliquam lectus pharetra eu. Donec velit elit, tincidunt semper mollis et, adipiscing vel dui. Morbi rhoncus dui sit amet libero gravida sagittis. Duis tincidunt luctus elit, ac cursus nisi tempus in. Fusce quis quam quam. Suspendisse hendrerit lobortis metus, non fermentum nibh tincidunt gravida.";

		public String mainGroup = "Friends";
		public List<String> groups = Lists.newArrayList("Friends", "Colleague");
		public List<String> emails = Lists.newArrayList("john.doe@gmail.com", "john.doe@aol.com");
	}

	public interface BeanModel extends Model<Bean> {

		Model<Bean> MODEL = GWT.create(BeanModel.class);
	}

	interface Binder extends UiBinderLocalized<Widget, FormsView> {
		Binder BINDER = GWT.create(Binder.class);
	}

	interface Constants extends ConstantsWithLookup, ValidationConstants {
		@DefaultStringValue("Name")
		String nameLabel();

		@DefaultStringValue("Enter your name")
		String namePlaceholder();

		@DefaultStringValue("Kindly message for name field")
		String nameHelp();

		@DefaultStringValue("Mr.")
		String genderMaleEnum();

		@DefaultStringValue("Mrs.")
		String genderFemaleEnum();
	}

	@UiField(provided = true)
	final NavSpy tableOfContent;

	@UiField
	Form<Bean> formBasic;
	@UiField
	Form<Bean> formInline;
	@UiField
	Form<Bean> formHorizontal;
	@UiField
	Form<Bean> formReadonly;
	@UiField
	Form<Bean> formHeaderFooter;

	@UiField
	Form<Bean> formFieldset;
	@UiField
	Fieldset<Bean> fieldsetIds;

	@UiField
	Form<Bean> formGroupEditable;
	@UiField
	Form<Bean> formGroupReadonly;
	@UiField
	Form<Bean> formGroupMagic;

	@UiConstructor
	public FormsView(NavSpy navSpy) {
		super();

		this.tableOfContent = navSpy;

		initWidget(Binder.BINDER.createAndBindUi(this));


		MessageHelper messageHelper = new MessageHelper((ConstantsWithLookup) GWT.create(Constants.class));

		formBasic.setMessageHelper(messageHelper);
		formBasic.initialize(BeanModel.MODEL);
		formBasic.getDriver().setAutoFlush(true);
		formBasic.edit(new Bean());

		formInline.setMessageHelper(messageHelper);
		formInline.initialize(BeanModel.MODEL);
		formInline.getDriver().setAutoFlush(true);
		formInline.edit(new Bean());

		formHorizontal.setMessageHelper(messageHelper);
		formHorizontal.initialize(BeanModel.MODEL);
		formHorizontal.getDriver().setAutoFlush(true);
		formHorizontal.edit(new Bean());

		formReadonly.setMessageHelper(messageHelper);
		formReadonly.initialize(BeanModel.MODEL);
		formReadonly.getDriver().setAutoFlush(true);
		formReadonly.edit(new Bean());

		formHeaderFooter.setMessageHelper(messageHelper);
		formHeaderFooter.initialize(BeanModel.MODEL);
		formHeaderFooter.getDriver().setAutoFlush(true);
		formHeaderFooter.edit(new Bean());

		formFieldset.setMessageHelper(messageHelper);
		formFieldset.initialize(BeanModel.MODEL);
		formFieldset.getDriver().setAutoFlush(true);
		formFieldset.edit(new Bean());

		formGroupEditable.setMessageHelper(messageHelper);
		formGroupEditable.initialize(BeanModel.MODEL);
		formGroupEditable.getDriver().setAutoFlush(true);
		formGroupEditable.edit(new Bean());

		formGroupReadonly.initialize(BeanModel.MODEL);
		formGroupReadonly.edit(new Bean());

		formGroupMagic.setMessageHelper(messageHelper);
		formGroupMagic.initialize(BeanModel.MODEL);
		formGroupMagic.edit(new Bean());

	}

	@UiHandler("fieldsetToogleReadOnly")
	public void onFieldsetToogleReadOnlyClick(ButtonEvent event) {
		fieldsetIds.setReadonly(Boolean.FALSE.equals(fieldsetIds.getReadonly()));
	}

}
