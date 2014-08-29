package fr.putnami.pwt.tutorial.shared.constants;

import fr.putnami.pwt.core.editor.shared.constant.ValidationConstants;

public interface IssueConstants extends ValidationConstants {

	@DefaultStringValue("ID")
	String id();

	@DefaultStringValue("Name")
	String name();

	@DefaultStringValue("Type")
	String type();

	@DefaultStringValue("Labels")
	String labels();

	@DefaultStringValue("Description")
	String description();

	/* Enum */

	@DefaultStringValue("Optional")
	String typeOptionalEnum();

	@DefaultStringValue("Minor")
	String typeMinorEnum();

	@DefaultStringValue("Major")
	String typeMajorEnum();

	@DefaultStringValue("Critical")
	String typeCriticalEnum();

	@DefaultStringValue("Blocker")
	String typeBlockerEnum();

	/* Placeholders */

	@DefaultStringValue("Simple name of the issue...")
	String namePlaceholder();

	@DefaultStringValue("Describe the issue here...")
	String descriptionPlaceholder();

	/* Tooltips */

	@DefaultStringValue("Enter the issue name")
	String nameTooltip();

	@DefaultStringValue("Select the issue type")
	String typeTooltip();

	@DefaultStringValue("Select the applying labels")
	String labelsTooltip();

	@DefaultStringValue("Enter the issue descriptionDescription")
	String descriptionTooltip();

	/* Help */

	@DefaultStringValue("Enter a short name of the issue.")
	String nameHelp();

	@DefaultStringValue("Select labels to apply to this issue.")
	String labelsHelp();

}
