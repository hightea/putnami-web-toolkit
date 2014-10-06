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
package fr.putnami.pwt.core.editor.shared.constant;

import com.google.gwt.i18n.client.ConstantsWithLookup;

public interface ValidationConstants extends ConstantsWithLookup {

	@DefaultStringValue("must be false")
	String constraintsAssertFalse();

	@DefaultStringValue("must be true")
	String constraintsAssertTrue();

	@DefaultStringValue("must be less than or equal to {0}")
	String constraintsDecimalMax();

	@DefaultStringValue("must be greater than or equal to {0}")
	String constraintsDecimalMin();

	@DefaultStringValue("must be in the future")
	String constraintsFuture();

	@DefaultStringValue("must be less than or equal to {0}")
	String constraintsMax();

	@DefaultStringValue("must be greater than or equal to {0}")
	String constraintsMin();

	@DefaultStringValue("may not be null")
	String constraintsNotNull();

	@DefaultStringValue("must be null")
	String constraintsNull();

	@DefaultStringValue("must be in the past")
	String constraintsPast();

	@DefaultStringValue("must match \"{0}\"")
	String constraintsPattern();

	@DefaultStringValue("size must be between {0} and {1}")
	String constraintsSize();

	@DefaultStringValue("must be a valid email address")
	String constraintsEmail();

	@DefaultStringValue("failed to parse the input {0}")
	String inputParsing();
}
