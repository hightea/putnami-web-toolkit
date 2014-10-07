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
package fr.putnami.pwt.core.widget.client;

import com.google.gwt.event.dom.client.KeyPressEvent;
import com.google.gwt.event.dom.client.KeyPressHandler;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.gwt.user.client.ui.TextBox;

import fr.putnami.pwt.core.editor.client.validator.EmailValidator;
import fr.putnami.pwt.core.widget.client.base.AbstractInputBox;
import fr.putnami.pwt.core.widget.client.helper.StringParser;
import fr.putnami.pwt.core.widget.client.helper.StringRenderer;

public class InputEmail extends AbstractInputBox<TextBox, String> {

	private static final KeyPressHandler EMAIL_CHAR_VALIDATOR = new KeyPressHandler() {

		@Override
		public void onKeyPress(KeyPressEvent event) {
			boolean valid = false;
			char pressed = event.getCharCode();
			if (pressed >= 'a' && pressed <= 'z' || pressed >= 'A' && pressed <= 'Z' || pressed >= '0'
				&& pressed <= '9' || pressed == '@' || pressed == '.' || pressed == '+' || pressed == '-'
				|| pressed == '_' || pressed == '%') {
				valid = true;
			}
			if (!valid) {
				event.preventDefault();
				event.stopPropagation();
			}
		}
	};

	public InputEmail() {
		super(new TextBox());

		this.setParser(StringParser.get());
		this.setRenderer(StringRenderer.get());

		this.addValidator(new EmailValidator());
		this.getInput().addKeyPressHandler(InputEmail.EMAIL_CHAR_VALIDATOR);
	}

	protected InputEmail(InputEmail source) {
		super(new TextBox(), source);
		this.getInput().addKeyPressHandler(InputEmail.EMAIL_CHAR_VALIDATOR);
	}

	@Override
	public IsWidget cloneWidget() {
		return new InputEmail(this);
	}

	@Override
	public String flush() {
		String email = super.flush();
		if (email != null) {
			email = email.toLowerCase();
			this.setValue(email);
		}
		return email;
	}
}
