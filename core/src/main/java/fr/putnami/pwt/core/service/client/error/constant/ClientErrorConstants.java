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
package fr.putnami.pwt.core.service.client.error.constant;

import com.google.gwt.i18n.client.ConstantsWithLookup;

/**
 * Client Errors default messages (sources http://en.wikipedia.org/wiki/List_of_HTTP_status_codes)
 */
public interface ClientErrorConstants extends ConstantsWithLookup {

	@DefaultStringValue("Client error")
	String clientErrorTitle();

	@DefaultStringValue("Bad Request")
	String clientError400();

	@DefaultStringValue("Unauthorized")
	String clientError401();

	@DefaultStringValue("Payment Required")
	String clientError402();

	@DefaultStringValue("Forbidden")
	String clientError403();

	@DefaultStringValue("Not Found")
	String clientError404();

	@DefaultStringValue("Method Not Allowed")
	String clientError405();

	@DefaultStringValue("Not Acceptable")
	String clientError406();

	@DefaultStringValue("Proxy Authentication Required")
	String clientError407();

	@DefaultStringValue("Request Timeout")
	String clientError408();

	@DefaultStringValue("Conflict")
	String clientError409();

	@DefaultStringValue("Gone")
	String clientError410();

	@DefaultStringValue("Length Required")
	String clientError411();

	@DefaultStringValue("Precondition Failed")
	String clientError412();

	@DefaultStringValue("Request Entity Too Large")
	String clientError413();

	@DefaultStringValue("Request-URI Too Long")
	String clientError414();

	@DefaultStringValue("Unsupported Media Type")
	String clientError415();

	@DefaultStringValue("Requested range Not Satisfiable")
	String clientError416();

	@DefaultStringValue("Expectation failed")
	String clientError417();

	@DefaultStringValue("I’m a teapot")
	String clientError418();

	@DefaultStringValue("Authentication Timeout")
	String clientError419();

	@DefaultStringValue("Enhance Your Calm")
	String clientError420();

	@DefaultStringValue("Unprocessable Entity")
	String clientError422();

	@DefaultStringValue("Locked")
	String clientError423();

	@DefaultStringValue("Failed Dependency")
	String clientError424();

	@DefaultStringValue("Unordered Collection")
	String clientError425();

	@DefaultStringValue("Upgrade Required")
	String clientError426();

	@DefaultStringValue("Precondition Required")
	String clientError428();

	@DefaultStringValue("Too Many Request")
	String clientError429();

	@DefaultStringValue("Request Header Fields Too Large")
	String clientError431();

	@DefaultStringValue("Login Timeout")
	String clientError440();

	@DefaultStringValue("No Response")
	String clientError444();

	@DefaultStringValue("Retry With")
	String clientError449();

	@DefaultStringValue("Blocked by Windows Parental Controls")
	String clientError450();

	@DefaultStringValue("Redirect")
	String clientError451();

	@DefaultStringValue("Request Header Too Large")
	String clientError494();

	@DefaultStringValue("Cert Error")
	String clientError495();

	@DefaultStringValue("No Cert")
	String clientError496();

	@DefaultStringValue("HTTP to HTTPS")
	String clientError497();

	@DefaultStringValue("Client Closed Request")
	String clientError499();
}
