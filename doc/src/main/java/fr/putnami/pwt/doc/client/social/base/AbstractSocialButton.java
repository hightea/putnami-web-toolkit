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
package fr.putnami.pwt.doc.client.social.base;

import com.google.gwt.core.shared.GWT;
import com.google.gwt.dom.client.Document;
import com.google.gwt.http.client.URL;
import com.google.gwt.user.client.Window;

import fr.putnami.pwt.core.editor.client.helper.MessageHelper;
import fr.putnami.pwt.core.widget.client.Button;
import fr.putnami.pwt.core.widget.client.event.ButtonEvent;
import fr.putnami.pwt.doc.client.social.constant.SocialConstants;

public abstract class AbstractSocialButton extends Button<Void> {

	protected static final SocialConstants CONSTANT = GWT.create(SocialConstants.class);

	private String socialUtlTemplate;
	private boolean asPopup = true;
	private int popupHeight = 400;
	private int popupWith = 700;

	public AbstractSocialButton() {
		this.setSize(Size.LARGE);
		this.addButtonHandler(new ButtonEvent.Handler() {
			@Override
			public void onButtonAction(ButtonEvent event) {
				AbstractSocialButton.this.socialize();
			}

		});
	}

	protected void socialize() {
		String url = this.buildSocialUrl();
		Window.open(url, "", this.buildPopupFeature());
	}

	protected String buildPopupFeature() {
		if (this.asPopup) {
			return "menubar=no,toolbar=no,resizable=yes,scrollbars=yes,height="
				+ this.popupHeight + ",width=" + this.popupWith;
		}
		return null;
	}

	protected String getPageUrl() {
		return AbstractSocialButton.CONSTANT.pwtUrl();
	}

	protected String buildSocialUrl() {
		return MessageHelper.replaceParams(this.socialUtlTemplate, this.getPageUrl(), URL
			.encode(Document.get().getTitle()));
	}

	public String getSocialUtlTemplate() {
		return this.socialUtlTemplate;
	}

	public void setSocialUtlTemplate(String socialUtlTemplate) {
		this.socialUtlTemplate = socialUtlTemplate;
	}

	public boolean isAsPopup() {
		return this.asPopup;
	}

	public void setAsPopup(boolean asPopup) {
		this.asPopup = asPopup;
	}

	public int getPopupHeight() {
		return this.popupHeight;
	}

	public void setPopupHeight(int popupHeight) {
		this.popupHeight = popupHeight;
	}

	public int getPopupWith() {
		return this.popupWith;
	}

	public void setPopupWith(int popupWith) {
		this.popupWith = popupWith;
	}

}
