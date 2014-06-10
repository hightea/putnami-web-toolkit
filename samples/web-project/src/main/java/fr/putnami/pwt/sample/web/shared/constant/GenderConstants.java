package fr.putnami.pwt.sample.web.shared.constant;

import com.google.gwt.i18n.client.ConstantsWithLookup;

public interface GenderConstants extends ConstantsWithLookup {
	@DefaultStringValue("Mr.")
	String genderMaleEnum();

	@DefaultStringValue("Mrs.")
	String genderFemaleEnum();
}
