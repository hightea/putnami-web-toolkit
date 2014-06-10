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
package fr.putnami.pwt.doc.client.page.form;

import com.google.gwt.core.client.GWT;
import com.google.gwt.i18n.client.ConstantsWithLookup;
import com.google.gwt.uibinder.client.UiConstructor;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.Widget;

import fr.putnami.pwt.core.editor.client.helper.MessageHelper;
import fr.putnami.pwt.core.editor.shared.constant.ValidationConstants;
import fr.putnami.pwt.core.model.client.model.Model;
import fr.putnami.pwt.core.widget.client.Form;
import fr.putnami.pwt.core.widget.client.NavSpy;
import fr.putnami.pwt.core.widget.client.binder.UiBinderLocalized;

public class FormGroupsView extends Composite {

	public static class Bean {
		public String name = "John Doe";
		public String email = "john.doe@gmail.com";
		public String password;
	}

	interface BeanModel extends Model<Bean> {

		Model<Bean> MODEL = GWT.create(BeanModel.class);
	}

	interface Binder extends UiBinderLocalized<Widget, FormGroupsView> {

		Binder BINDER = GWT.create(Binder.class);
	}

	public interface Constants extends ConstantsWithLookup, ValidationConstants {
		@DefaultStringValue("Name")
		String nameLabel();

		@DefaultStringValue("Enter your name")
		String namePlaceholder();

		@DefaultStringValue("Kindly message for name field")
		String nameHelp();
	}

	@UiField(provided = true)
	final NavSpy navSpy;

	@UiField
	Form<Bean> formEditable;
	@UiField
	Form<Bean> formReadonly;
	@UiField
	Form<Bean> formMagic;

	@UiConstructor
	public FormGroupsView(NavSpy navSpy) {
		this.navSpy = navSpy;

		initWidget(Binder.BINDER.createAndBindUi(this));

		MessageHelper messageHelper = new MessageHelper((ConstantsWithLookup) GWT.create(Constants.class));

		formEditable.setMessageHelper(messageHelper);
		formEditable.initialize(BeanModel.MODEL);
		formEditable.getDriver().setAutoFlush(true);
		formEditable.edit(new Bean());

		formReadonly.initialize(BeanModel.MODEL);
		formReadonly.edit(new Bean());

		formMagic.setMessageHelper(messageHelper);
		formMagic.initialize(BeanModel.MODEL);
		formMagic.edit(new Bean());

		navSpy.redraw();
	}

}
