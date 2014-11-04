package fr.putnami.pwt.sample.rest.shared.constants;

import fr.putnami.pwt.core.editor.shared.constant.ValidationConstants;

public interface UserConstants extends ValidationConstants {

	@DefaultStringValue("ID")
	String id();

	@DefaultStringValue("Firstname")
	String firstname();

	@DefaultStringValue("Lastname")
	String lastname();

	@DefaultStringValue("Avatar")
	String avatar();

	@DefaultStringValue("Job")
	String job();

	@DefaultStringValue("Created At")
	String createdat();

	@DefaultStringValue("Modified At")
	String updatedat();
}
