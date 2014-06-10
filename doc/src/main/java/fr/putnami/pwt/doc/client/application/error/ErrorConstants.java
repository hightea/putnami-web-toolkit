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
package fr.putnami.pwt.doc.client.application.error;

import com.google.gwt.i18n.client.ConstantsWithLookup;

public interface ErrorConstants extends ConstantsWithLookup {

	@DefaultStringValue("Unknown error (Runtime Exception)")
	String runtimeExceptionTitle();

	@DefaultStringValue("An unknown exception (Runtime Exception) occured. Please contact technical services if this Error is blocking")
	String runtimeExceptionMessage();

	@DefaultStringValue("Custom Runtime Error")
	String customRuntimeExceptionTitle();

	@DefaultStringValue("An custom runtime exception occured. Please contact technical services if this Error is blocking")
	String customRuntimeExceptionMessage();
}
