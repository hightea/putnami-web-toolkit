package fr.putnami.pwt.tutorial.shared.constants;

import com.google.gwt.i18n.client.ConstantsWithLookup;

public interface ContactConstants extends ConstantsWithLookup {

	@DefaultStringValue("Name")
	String name();

	@DefaultStringValue("Email")
	String email();

	@DefaultStringValue("Birthday")
	String birthday();

	@DefaultStringValue("Tutorial note")
	String tutorialnote();

	@DefaultStringValue("Subject")
	String subject();

	@DefaultStringValue("Message")
	String message();

	/* Placeholders */

	@DefaultStringValue("Your name...")
	String namePlaceholder();

	@DefaultStringValue("Your email...")
	String emailPlaceholder();

	@DefaultStringValue("Your birthday...")
	String birthdayPlaceholder();

	@DefaultStringValue("Your message...")
	String messagePlaceholder();

	/* Tooltips */

	@DefaultStringValue("Enter your name")
	String nameTooltip();

	@DefaultStringValue("Enter your email")
	String emailTooltip();

	@DefaultStringValue("Enter your birthday")
	String birthdayTooltip();

	@DefaultStringValue("Select a note")
	String tutorialnoteTooltip();

	@DefaultStringValue("Select a subject")
	String subjectTooltip();

	@DefaultStringValue("Enter your message")
	String messageTooltip();

	/* Help */

	@DefaultStringValue("Enter your full name (Lastname and Firstname)")
	String nameHelp();

	@DefaultStringValue("Enter your email so we can contact you back")
	String emailHelp();

	@DefaultStringValue("Enter your birthday, maybe you'll have some present")
	String birthdayHelp();

	@DefaultStringValue("Select a note for this tutorial")
	String tutorialnoteHelp();

	@DefaultStringValue("Select a subject for your message")
	String subjectHelp();

	@DefaultStringValue("Enter your message, we will read it!")
	String messageHelp();
}
