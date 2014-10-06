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
package fr.putnami.pwt.core.service.client.error.constant;

import com.google.gwt.i18n.client.ConstantsWithLookup;

/**
 * Server Errors default messages (sources http://en.wikipedia.org/wiki/List_of_HTTP_status_codes).
 */
public interface ServerErrorConstants extends ConstantsWithLookup {

	@DefaultStringValue("Server error")
	String serverErrorTitle();

	@DefaultStringValue("Internal Server Error")
	String serverError500();

	@DefaultStringValue("Not Implemented")
	String serverError501();

	@DefaultStringValue("Bad Gateway")
	String serverError502();

	@DefaultStringValue("Service Unavailable")
	String serverError503();

	@DefaultStringValue("Gateway Time-out")
	String serverError504();

	@DefaultStringValue("HTTP Version not supported")
	String serverError505();

	@DefaultStringValue("Variant also negociate")
	String serverError506();

	@DefaultStringValue("Insufficient storage")
	String serverError507();

	@DefaultStringValue("Loop detected")
	String serverError508();

	@DefaultStringValue("Bandwidth Limit Exceeded")
	String serverError509();

	@DefaultStringValue("Not extended")
	String serverError510();

	@DefaultStringValue("Network Authentication Required")
	String serverError511();

	@DefaultStringValue("Origin Error")
	String serverError520();

	@DefaultStringValue("Web server is down")
	String serverError521();

	@DefaultStringValue("Connection timed out")
	String serverError522();

	@DefaultStringValue("Proxy Declined Request")
	String serverError523();

	@DefaultStringValue("A timeout occured")
	String serverError524();

	@DefaultStringValue("Network read timeout error")
	String serverError598();

	@DefaultStringValue("Network connect timeout error")
	String serverError599();
}
