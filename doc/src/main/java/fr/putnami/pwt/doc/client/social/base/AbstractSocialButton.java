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
    setSize(Size.LARGE);
    addButtonHandler(new ButtonEvent.Handler() {
      @Override
      public void onButtonAction(ButtonEvent event) {
        socialize();
      }

    });
  }

  protected void socialize() {
    String url = buildSocialUrl();
    Window.open(url, "", buildPopupFeature());
  }

  protected String buildPopupFeature() {
    if (asPopup) {
      return "menubar=no,toolbar=no,resizable=yes,scrollbars=yes,height=" + popupHeight + ",width=" + popupWith;
    }
    return null;
  }

  protected String getPageUrl(){
    return CONSTANT.pwtUrl();
    //		String page = CONSTANT.pwtUrl();
    //		String historyToken = History.getToken();
    //		if (!Strings.isNullOrEmpty(historyToken)) {
    //			page += "#" + historyToken;
    //		}
    //		return URL.encode(page);
  }

  protected String buildSocialUrl(){
    return MessageHelper.replaceParams(socialUtlTemplate,
        getPageUrl(),
        URL.encode(Document.get().getTitle())
        );
  }

  public String getSocialUtlTemplate() {
    return socialUtlTemplate;
  }

  public void setSocialUtlTemplate(String socialUtlTemplate) {
    this.socialUtlTemplate = socialUtlTemplate;
  }

  public boolean isAsPopup() {
    return asPopup;
  }

  public void setAsPopup(boolean asPopup) {
    this.asPopup = asPopup;
  }

  public int getPopupHeight() {
    return popupHeight;
  }

  public void setPopupHeight(int popupHeight) {
    this.popupHeight = popupHeight;
  }

  public int getPopupWith() {
    return popupWith;
  }

  public void setPopupWith(int popupWith) {
    this.popupWith = popupWith;
  }

}
