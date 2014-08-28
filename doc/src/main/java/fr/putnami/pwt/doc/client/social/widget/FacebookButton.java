package fr.putnami.pwt.doc.client.social.widget;

import fr.putnami.pwt.doc.client.social.base.AbstractSocialButton;

public class FacebookButton extends AbstractSocialButton {

	public FacebookButton() {
		setIconType(CONSTANT.facebookIcon());
		setTitle(CONSTANT.facebookPopover());
		setSocialUtlTemplate(CONSTANT.facebookUrl());
		setPopupHeight(500);
		setPopupWith(700);
	}
}
