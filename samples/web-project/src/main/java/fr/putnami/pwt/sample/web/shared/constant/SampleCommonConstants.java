package fr.putnami.pwt.sample.web.shared.constant;

import fr.putnami.pwt.core.editor.shared.constant.ValidationConstants;

public interface SampleCommonConstants extends ValidationConstants {

	@DefaultStringValue("Save")
	String saveButton();

	@DefaultStringValue("Cancel")
	String cancelButton();

	@DefaultStringValue("Submit")
	String submitButton();

	@DefaultStringValue("Close")
	String closeButton();

	@DefaultStringValue("Open")
	String openButton();

	@DefaultStringValue("Add")
	String addButton();
}
