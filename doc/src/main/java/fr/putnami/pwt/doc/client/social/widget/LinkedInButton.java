package fr.putnami.pwt.doc.client.social.widget;

import fr.putnami.pwt.doc.client.social.base.AbstractSocialButton;

public class LinkedInButton extends AbstractSocialButton {

	public LinkedInButton() {
		this.setIconType(AbstractSocialButton.CONSTANT.linkedinIcon());
		this.setTitle(AbstractSocialButton.CONSTANT.linkedinPopover());
		this.setSocialUtlTemplate(AbstractSocialButton.CONSTANT.linkedinUrl());
		this.setPopupWith(500);
	}
}
