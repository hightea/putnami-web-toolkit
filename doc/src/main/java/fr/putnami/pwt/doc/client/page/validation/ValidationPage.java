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
package fr.putnami.pwt.doc.client.page.validation;

import com.google.gwt.uibinder.client.UiField;

import java.util.Date;

import javax.validation.constraints.AssertTrue;
import javax.validation.constraints.Max;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Past;
import javax.validation.constraints.Pattern;

import fr.putnami.pwt.core.editor.shared.constant.ValidationConstants;
import fr.putnami.pwt.core.inject.client.annotation.Initialize;
import fr.putnami.pwt.core.inject.client.annotation.PostConstruct;
import fr.putnami.pwt.core.inject.client.annotation.Templated;
import fr.putnami.pwt.core.mvp.client.ViewPlace;
import fr.putnami.pwt.core.mvp.client.annotation.ActivityDescription;
import fr.putnami.pwt.core.widget.client.Form;
import fr.putnami.pwt.doc.client.application.Page;
import fr.putnami.pwt.doc.client.application.SummaryDecorator;

@Templated
public class ValidationPage extends Page {
	@ActivityDescription(view = ValidationPage.class, viewDecorator = SummaryDecorator.class)
	public static class ValidationPlace extends ViewPlace {
	}

	public static class BeanToValidate {
		@Past
		@NotNull
		public Date birthday;

		@Max(12)
		public Integer monthNumber;

		@AssertTrue
		public Boolean agree;

		@Pattern(regexp = "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\\\.[a-zA-Z]{2,4}$")
		public String email;
	}

	public interface Constants extends ValidationConstants {
		@DefaultStringValue("Birth date")
		String birthday();

		@DefaultStringValue("Date of birth...")
		String birthdayPlaceholder();

		@DefaultStringValue("Number of month until new year's eve")
		String monthnumber();

		@DefaultStringValue("E-mail")
		String email();

		@DefaultStringValue("E-mail...")
		String emailPlaceholder();

		@DefaultStringValue("I've tried this")
		String agree();
	}

	@UiField
	@Initialize(constantsClass = Constants.class)
	Form<BeanToValidate> validationForm;

	@PostConstruct
	public void postConstruct() {
		// this.validationForm.edit(new ValidationForm());
	}

}
