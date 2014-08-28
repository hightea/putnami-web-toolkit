package fr.putnami.pwt.doc.client.social.widget;

import fr.putnami.pwt.doc.client.social.base.AbstractSocialButton;

public class LinkedInButton extends AbstractSocialButton {

	public LinkedInButton() {
		setIconType(CONSTANT.linkedinIcon());
		setTitle(CONSTANT.linkedinPopover());
		setSocialUtlTemplate(CONSTANT.linkedinUrl());
		setPopupWith(500);
	}
}
