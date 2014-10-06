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

import com.google.common.collect.Lists;
import com.google.gwt.i18n.client.ConstantsWithLookup;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.user.client.Random;
import com.google.gwt.user.client.ui.Composite;

import java.util.Date;
import java.util.List;

import fr.putnami.pwt.core.editor.shared.constant.ValidationConstants;
import fr.putnami.pwt.core.inject.client.annotation.Initialize;
import fr.putnami.pwt.core.inject.client.annotation.PostConstruct;
import fr.putnami.pwt.core.inject.client.annotation.Templated;
import fr.putnami.pwt.core.mvp.client.View;
import fr.putnami.pwt.core.widget.client.Form;
import fr.putnami.pwt.core.widget.client.helper.DateParser;
import fr.putnami.pwt.core.widget.shared.domain.FileDto;

@Templated
public class InputControlsView extends Composite implements View {

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

		public FileDto file;
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
	final List<String> stateSuggestions = Lists.newArrayList("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut",
			"Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland",
			"Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey",
			"New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
			"South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming");

	@UiField(provided = true)
	final List<Integer> ageList = generateAgeList();
	@UiField(provided = true)
	final List<String> groups = Lists.newArrayList("Familly", "Friends", "Colleague", "Other");

	@UiField
	@Initialize(constantsClass = Constants.class)
	Form<Bean> formInputEditor;

	@PostConstruct
	public void postConstruct() {
		formInputEditor.edit(new Bean());
		formInputEditor.getDriver().setAutoFlush(true);
	}

	private List<Integer> generateAgeList() {
		List<Integer> result = Lists.newArrayList();
		for (int i = 0; i <= 100; i++) {
			result.add(Integer.valueOf(i));
		}
		return result;
	}

}
