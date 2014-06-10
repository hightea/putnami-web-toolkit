package fr.putnami.pwt.sample.web.shared.constant;

public interface ContactConstants extends PersonConstants {

	@DefaultStringValue("Emails")
	String emails();

	@DefaultStringValue("Phones")
	String phones();

	@DefaultStringValue("Groups")
	String groups();

	@DefaultStringValue("Your comments...")
	String remarksPlaceholder();

	@Override
	@DefaultStringValue("Contat name...")
	String namePlaceholder();

}
